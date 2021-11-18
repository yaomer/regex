#ifndef _REGEX_H
#define _REGEX_H

#include <vector>
#include <string>
#include <stack>
#include <unordered_map>
#include <unordered_set>

namespace regex {

// 我们使用Thompson算法构造一个NFA，其构造出的NFA有以下特点：
// 1) N(r)(由表达式r构成的NFA)有且只有一个开始状态和接受状态，开始状态没有入边，接受状态没有出边。
// 2) N(r)中除接受状态以外的每个状态要么有一条symbol出边，要么至多有两条epsilon出边。
// *) 我们加入[]支持后，一条symbol出边上可能会有多个symbol，不过它们都转向同一个状态。

struct nfa_state {
    enum { Single, Charset, CharsetNot }; // single-symbol-trans, [], [^]
    enum { AnchorNone, AnchorStart, AnchorEnd }; // ^ $
    nfa_state(bool isend) : isend(isend) {  }
    bool is_anchor_start() { return anchor_type == AnchorStart; }
    bool is_anchor_end() { return anchor_type == AnchorEnd; }
    void add_epsilon_trans(nfa_state *to)
    {
        epsilon_edges.emplace_back(to);
    }
    void add_symbol_trans(char symbol, nfa_state *to)
    {
        symbols.emplace(symbol);
        this->to = to;
    }
    bool can_symbol_trans(char symbol)
    {
        assert(!isend);
        switch (symbol_type) {
        case Charset: return symbols.count(symbol);
        case CharsetNot: return !symbols.count(symbol);
        case Single:
            if (symbols.empty()) return false;
            return (*symbols.begin() == '.' && symbol != '\n') || (*symbols.begin() == symbol);
        default: assert(0);
        }
    }
    bool isend; // 是否为接受状态
    std::vector<nfa_state*> epsilon_edges; // 1 or 2
    int symbol_type = Single;
    int anchor_type = AnchorNone;
    std::unordered_set<char> symbols;
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
    NFA(char symbol, int symbol_type) : start(new nfa_state(false)), end(new nfa_state(true))
    {
        start->add_symbol_trans(symbol, end);
        start->symbol_type = symbol_type;
    }
    void set_anchor_start()
    {
        start->anchor_type = nfa_state::AnchorStart;
    }
    void set_anchor_end()
    {
        end->anchor_type = nfa_state::AnchorEnd;
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
// 由于正则表达式的(|*?+)等基本运算符都是右结合的，因此我们可以很容易写出它的文法，
// 并用递归下降的方法来解析生成AST。
//
// expr -> anchor '|' expr
//       | anchor
// anchor -> '^' anchor
//         | term '$'
//         | term
// term -> factor term
//       | factor
// factor -> atom meta-char
//         | atom
// atom -> char | (expr) | [charset]
//
// char -> any-char-except-meta
//       | '\'any-char
// meta-char -> '*' | '?' | '+'
//
// <expr> for union
// <anchor> for '^' '$'
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
        Expr = 256, Anchor, Term, Factor, Atom, Char
    };

    struct tree_node {
        tree_node(int type) : type(type) {  }
        tree_node(int type, std::initializer_list<tree_node> il)
            : type(type), childs(il) {  }
        int type;
        int anchor = 0;
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
            nfa = build(node.childs[0]); // expr -> anchor
            if (node.childs.size() == 3) nfa.Union(build(node.childs[2])); // expr -> anchor '|' expr
            break;
        case Anchor:
            if (node.childs[0].type == Term) {
                nfa = build(node.childs[0]); // anchor -> term
                if (node.childs.size() == 2) { // anchor -> term '$'
                    nfa.set_anchor_end();
                }
            } else { // anchor -> '^' anchor
                nfa = build(node.childs[1]);
                nfa.set_anchor_start();
            }
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
            if (node.childs.size() == 1) {
                nfa = build(node.childs[0]); // atom -> char
            } else if (node.childs[0].type == '(') {
                nfa = build(node.childs[1]); // atom -> (expr)
            } else { // atom -> [charset]
                nfa = build_from_charset(node);
            }
            break;
        case Char:
            if (node.childs.size() == 1) { // char -> any-char-except-meta
                nfa = NFA(node.childs[0].type);
            } else { // char -> '\'any-char
                return build_from_escape(node.childs[1].type);
            }
            break;
        default:
            assert(0);
        }
        return nfa;
    }
    NFA build_from_charset(tree_node& node)
    {
        NFA nfa;
        int i = 2;
        if (node.childs[1].type == '^') {
            if (node.childs.size() > 3) nfa = NFA(node.childs[2].type, nfa_state::CharsetNot);
            else nfa = NFA('\0', nfa_state::CharsetNot); // [^]
            i++;
        } else {
            nfa = NFA(node.childs[1].type, nfa_state::Charset);
        }
        for ( ; i < node.childs.size() - 1; i++) {
            nfa.start->symbols.emplace(node.childs[i].type);
        }
        return nfa;
    }
    NFA build_from_escape(char c)
    {
        static const char *digit = "0123456789";
        static const char *word = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";
        static const char *space = " \t\r\n\v\f";
        bool charset_not = false;
        const char *s;
        switch (c) {
        case 'd': s = digit; break;
        case 'D': s = digit; charset_not = true; break;
        case 'w': s = word; break;
        case 'W': s = word; charset_not = true; break;
        case 's': s = space; break;
        case 'S': s = space; charset_not = true; break;
        default: return NFA(c);
        }
        NFA nfa(*s++, charset_not ? nfa_state::CharsetNot : nfa_state::Charset);
        while (*s) nfa.start->symbols.emplace(*s++);
        return nfa;
    }
    // expr -> anchor '|' expr
    //       | anchor
    tree_node expr()
    {
        auto anchor_node = anchor();
        if (!eof() && peek() == '|') {
            match('|');
            return tree_node(Expr, { anchor_node, tree_node('|'), expr() });
        }
        return tree_node(Expr, { anchor_node });
    }
    // anchor -> '^' anchor
    //         | term '$'
    //         | term
    tree_node anchor()
    {
        if (!eof() && peek() == '^') {
            match('^');
            return tree_node(Anchor, { tree_node('^'), anchor() });
        }
        auto term_node = term();
        if (!eof() && peek() == '$') {
            match('$');
            return tree_node(Anchor, { term_node, tree_node('$') });
        }
        return tree_node(Anchor, { term_node });
    }
    // term -> factor term
    //       | factor
    tree_node term()
    {
        auto factor_node = factor();
        // expr() needs to match '|'
        // atom() needs to match ')'
        // anchor() needs to match '$'
        if (!eof() && peek() != '|' && peek() != ')' && peek() != '$') {
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
    // atom -> char | (expr) | [charset]
    tree_node atom()
    {
        if (peek() == '(') {
            match('(');
            auto expr_node = expr();
            match(')');
            return tree_node(Atom, { tree_node('('), expr_node, tree_node(')') });
        }
        if (peek() == '[') {
            return charset();
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
    // expand like [a-z]
    tree_node charset()
    {
        match('[');
        if (eof()) throw parse_error;
        tree_node charset_node(Atom);
        charset_node.childs.emplace_back('[');
        while (peek() != ']') {
            char c = peek();
            if (c == '\\') match('\\');
            charset_node.childs.emplace_back(nextchar());
            if (eof()) throw parse_error;
            char c2 = peek();
            if (c2 == ']') break;
            if (c2 != '-') continue;
            nextchar();
            if (eof()) throw parse_error;
            char c3 = peek();
            if (c3 == ']') {
                charset_node.childs.emplace_back(c2);
                break;
            }
            if (c > c3) throw parse_error;
            for (char cur = c + 1; cur <= c3; cur++)
                charset_node.childs.emplace_back(cur);
        }
        match(']');
        charset_node.childs.emplace_back(']');
        return charset_node;
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
        if (nfa.start->is_anchor_start())
            return search(nfa.start, s, 0);
        for (int i = 0; i < s.size(); i++)
            if (search(nfa.start, s, i))
                return true;
        return false;
    }
private:
    // 朴素的DFS，最坏时间复杂度为O(2^n)
    bool search(nfa_state *state, const std::string& s, int i)
    {
        if (i == s.size()) {
            if (state->isend) return true;
            for (auto& state : state->epsilon_edges) {
                if (search(state, s, i)) return true;
            }
        } else {
            if (state->isend) return !state->is_anchor_end();
            if (((state->is_anchor_start() && i == 0) || !state->is_anchor_start()) && state->can_symbol_trans(s[i])) {
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
