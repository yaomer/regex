#ifndef _REGEX_H
#define _REGEX_H

#include <vector>
#include <string>
#include <stack>
#include <unordered_map>

namespace regex {

// 我们使用Thompson算法构造一个NFA，其构造出的NFA有以下特点：
// 1) N(r)(由表达式r构成的NFA)有且只有一个开始状态和接受状态，开始状态没有入边，接受状态没有出边。
// 2) N(r)中除接受状态以外的每个状态要么有一条symbol出边，要么至多有两条epsilon出边。

struct nfa_state {
    nfa_state(bool isend) : isend(isend) {  }
    void add_epsilon_trans(nfa_state *to)
    {
        epsilon_edges.emplace_back(to);
    }
    void add_symbol_trans(char symbol, nfa_state *to)
    {
        this->symbol = symbol;
        this->to = to;
    }
    bool isend; // 是否为接受状态
    std::vector<nfa_state*> epsilon_edges; // 1 or 2
    char symbol = '\0';
    nfa_state *to = nullptr;
};

struct NFA {
    NFA() : start(new nfa_state(false)), end(new nfa_state(true))
    {
        start->add_epsilon_trans(end);
    }
    NFA(char symbol) : start(new nfa_state(false)), end(new nfa_state(true))
    {
        start->add_symbol_trans(symbol, end);
    }
    void concat(NFA nfa)
    {
        end->add_epsilon_trans(nfa.start);
        end->isend = false;
        end = nfa.end;
    }
    void Union(NFA nfa)
    {
        auto new_start = new nfa_state(false);
        auto new_end = new nfa_state(true);

        new_start->add_epsilon_trans(start);
        new_start->add_epsilon_trans(nfa.start);

        end->add_epsilon_trans(new_end);
        nfa.end->add_epsilon_trans(new_end);
        end->isend = false;
        nfa.end->isend = false;

        start = new_start;
        end = new_end;
    }
    void closure(bool plus = false)
    {
        auto new_start = new nfa_state(false);
        auto new_end = new nfa_state(true);

        new_start->add_epsilon_trans(start);
        if (!plus) new_start->add_epsilon_trans(new_end);

        end->add_epsilon_trans(start);
        end->add_epsilon_trans(new_end);
        end->isend = false;

        start = new_start;
        end = new_end;
    }
    void zero_or_one() // ?
    {
        start->add_epsilon_trans(end);
    }
    void one_or_more() // +
    {
        closure(true);
    }
    // 我们暂时不考虑释放内存
    nfa_state *start, *end;
};

//====================================================//

struct builder {
    virtual bool build(NFA& nfa, const std::string& expr) = 0;
    virtual ~builder() = default;
};

// 通过后缀表达式来构建NFA
struct postfix_builder : builder {
    bool build(NFA& nfa, const std::string& expr) override
    {
        nfa = build(expr);
        return true;
    }
private:
    NFA build(const std::string& expr)
    {
        NFA nfa;
        std::stack<NFA> stk;
        auto postfix = to_postfix(add_concat_operator(expr));
        for (auto c : postfix) {
            switch (c) {
            case '.':
                nfa = stk.top(); stk.pop();
                stk.top().concat(nfa);
                break;
            case '|':
                nfa = stk.top(); stk.pop();
                stk.top().Union(nfa);
                break;
            case '*':
                stk.top().closure();
                break;
            case '?':
                stk.top().zero_or_one();
                break;
            case '+':
                stk.top().one_or_more();
                break;
            default:
                stk.push(NFA(c));
                break;
            }
        }
        assert(stk.size() == 1);
        return stk.top();
    }
    // 添加显式的联结符'.' (ab -> a.b)
    // (ps: 所以我们无法处理元字符'.'了)
    // REQ: 合法的正则表达式expr
    std::string add_concat_operator(const std::string& expr)
    {
        std::string res;
        int n = expr.size();
        for (int i = 0; i < n; i++) {
            char c = expr[i];
            res.push_back(c);
            if (c == '(' || c == '|') continue;
            if (i + 1 < n) {
                if (strchr("*?+|)", expr[i+1])) continue;
                res.push_back('.');
            }
        }
        return res;
    }
    // 将中缀表达式转为后缀表达式
    // REQ: 合法的中缀表达式
    std::string to_postfix(const std::string& expr)
    {
        static std::unordered_map<char, int> op_prior = {
            { '|' , 0 },
            { '.' , 1 },
            { '?' , 2 },
            { '*' , 2 },
            { '+' , 2 },
        };
        std::string res;
        std::stack<char> stk;
        for (auto c : expr) {
            if (c == '|' || c == '.' || c == '?' || c == '*' || c == '+') {
                while (!stk.empty() && stk.top() != '(' && op_prior[stk.top()] >= op_prior[c]) {
                    res.push_back(stk.top());
                    stk.pop();
                }
                stk.push(c);
            } else if (c == '(') {
                stk.push('(');
            } else if (c == ')') {
                while (!stk.empty() && stk.top() != '(') {
                    res.push_back(stk.top());
                    stk.pop();
                }
                stk.pop();
            } else {
                res.push_back(c);
            }
        }
        while (!stk.empty()) {
            res.push_back(stk.top());
            stk.pop();
        }
        return res;
    }
};

// 这里我们通过生成抽象语法树(AST)来构建NFA
//
// 我们用BNF来描述我们的正则表达式，基本思路就是为每个优先级的运算符都引入一个产生式
// 优先级越高的应该越靠后，也即它会出现在AST中越底部的位置。
//
// 由于正则表达式的|*?+等基本运算符都是右结合的，因此我们可以很容易写出它的文法，
// 并用递归下降的方法来解析生成AST。
//
// expr -> term '|' expr
//       | term
// term -> factor term
//       | factor
// factor -> atom meta-char
//         | atom
// atom -> char | (expr)
//
// char -> any-char-except-meta
//       | '\'any-char
// meta-char -> '*' | '?' | '+'
//
// <expr> for union
// <term> for concat
// <factor> for meta-char
// <atom>为正则表达式执行的基本单元，是不可分割的。
// <char>用于区分转义字符和非转义字符。
//
struct ast_builder : builder {
    bool build(NFA& nfa, const std::string& expr) override
    {
        p = expr.data(), end = p + expr.size();
        try {
            auto root = this->expr();
            nfa =  build(root);
        } catch (parse_error_code& code) {
            return false;
        }
        return true;
    }
private:
    enum node_type {
        Expr = 256, Term, Factor, Atom, Char
    };

    struct tree_node {
        tree_node(int type) : type(type) {  }
        tree_node(int type, std::initializer_list<tree_node> il)
            : type(type), childs(il) {  }
        int type;
        std::vector<tree_node> childs;
    };

    typedef const int parse_error_code;
    static parse_error_code parse_error = -1;

    // 沿AST自顶向下构建NFA
    NFA build(tree_node& node)
    {
        NFA nfa;
        switch (node.type) {
        case Expr:
            nfa = build(node.childs[0]); // expr -> term
            if (node.childs.size() == 3) nfa.Union(build(node.childs[2])); // expr -> term '|' expr
            break;
        case Term:
            nfa = build(node.childs[0]); // term -> factor
            if (node.childs.size() == 2) nfa.concat(build(node.childs[1])); // term -> factor term
            break;
        case Factor:
            nfa = build(node.childs[0]); // factor -> atom
            if (node.childs.size() == 2) { // factor -> atom meta-char
                int metachar = node.childs[1].type;
                switch (metachar) {
                case '*': nfa.closure(); break;
                case '?': nfa.zero_or_one(); break;
                case '+': nfa.one_or_more(); break;
                }
            }
            break;
        case Atom:
            if (node.childs.size() == 1) nfa = build(node.childs[0]); // atom -> char
            else nfa = build(node.childs[1]); // atom -> (expr)
            break;
        case Char:
            if (node.childs.size() == 1) nfa = NFA(node.childs[0].type); // char -> any-char-except-meta
            else nfa = NFA(node.childs[1].type); // char -> '\'any-char
            break;
        default:
            assert(0);
        }
        return nfa;
    }
    // expr -> term '|' expr
    //       | term
    tree_node expr()
    {
        auto term_node = term();
        if (!eof() && peek() == '|') {
            match('|');
            return tree_node(Expr, { term_node, tree_node('|'), expr() });
        }
        return tree_node(Expr, { term_node });
    }
    // term -> factor term
    //       | factor
    tree_node term()
    {
        auto factor_node = factor();
        // expr() needs to match '|'
        // atom() needs to match ')'
        if (!eof() && peek() != '|' && peek() != ')') {
            return tree_node(Term, { factor_node, term() });
        }
        return tree_node(Term, { factor_node });
    }
    // factor -> atom meta-char
    //         | atom
    tree_node factor()
    {
        auto atom_node = atom();
        if (!eof() && ismetachar(peek())) {
            return tree_node(Factor, { atom_node, tree_node(nextchar()) });
        }
        return tree_node(Factor, { atom_node });
    }
    // atom -> char | (expr)
    tree_node atom()
    {
        if (peek() == '(') {
            match('(');
            auto expr_node = expr();
            match(')');
            return tree_node(Atom, { tree_node('('), expr_node, tree_node(')') });
        }
        return tree_node(Atom, { character() });
    }
    tree_node character()
    {
        if (ismetachar(peek())) throw parse_error;
        if (peek() == '\\') {
            match('\\');
            return tree_node(Char, { tree_node('\\'), tree_node(nextchar()) });
        }
        return tree_node(Char, { tree_node(nextchar()) });
    }

    int peek() { return *p; }
    int nextchar() { return *p++; }
    bool eof() { return p == end; }
    bool ismetachar(int c) { return strchr("*?+", c); }
    void match(int c)
    {
        if (peek() != c) throw parse_error;
        nextchar();
    }
    const char *p, *end;
};

class matcher {
public:
    matcher() : builder(new ast_builder()) {  }
    matcher(const matcher&) = delete;
    matcher& operator=(const matcher&) = delete;
    bool build(const std::string& expr)
    {
        if (expr.empty()) return false;
        return builder->build(nfa, expr);
    }
    bool match(const std::string& s)
    {
        for (int i = 0; i < s.size(); i++)
            if (search(nfa.start, s, i))
                return true;
        return false;
    }
private:
    // 朴素的DFS，最坏时间复杂度为O(2^n)
    bool search(nfa_state *state, const std::string& s, int i)
    {
        if (state->isend) return true;
        if (i == s.size()) {
            for (auto& state : state->epsilon_edges) {
                if (search(state, s, i)) return true;
            }
        } else {
            if ((state->symbol == '.' && s[i] != '\n') || state->symbol == s[i]) {
                if (search(state->to, s, i + 1)) return true;
            } else {
                for (auto& state : state->epsilon_edges) {
                    if (search(state, s, i)) return true;
                }
            }
        }
        return false;
    }
    std::unique_ptr<builder> builder;
    NFA nfa;
};

}

#endif // _REGEX_H
