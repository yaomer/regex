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
    void concat(NFA& nfa)
    {
        end->add_epsilon_trans(nfa.start);
        end->isend = false;
        end = nfa.end;
    }
    void Union(NFA& nfa)
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
    void zero_or_one()
    {
        start->add_epsilon_trans(end);
    }
    void one_or_more()
    {
        closure(true);
    }
    // 我们暂时不考虑释放内存
    nfa_state *start, *end;
};

struct builder {
    virtual NFA build(const std::string& expr) = 0;
    virtual ~builder() = default;
};

struct postfix_builder : builder {
    NFA build(const std::string& expr) override
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
private:
    // 添加显式的联结符'.'
    // ab -> a.b
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

class matcher {
public:
    matcher() : builder(new postfix_builder()) {  }
    matcher(const matcher&) = delete;
    matcher& operator=(const matcher&) = delete;
    void build(const std::string& expr)
    {
        nfa = builder->build(expr);
    }
    bool match(const std::string& s)
    {
        return search(nfa.start, s, 0);
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
            if (state->symbol == s[i]) {
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
