#pragma once
#include <functional>
#include "intrusive_list.h"

namespace signals
{

template <typename T>
struct signal;

template <typename... Args>
struct signal<void (Args...)>
{
    struct connection_tag;
    struct connection;
    struct iteration_token;

    using connections_t = intrusive::list<connection, connection_tag>;
    using slot_t = std::function<void(Args...)>;

private:
    connections_t connections;
    mutable iteration_token* top_token = nullptr;

public:
    signal() = default;
    signal(signal const&) = delete;
    signal& operator=(signal const&) = delete;

    connection connect(std::function<void (Args...)> slot) noexcept {
        return connection(this, std::move(slot));
    }

    void operator()(Args... args) const {
        iteration_token t(this);
        while (t.iterator != connections.end()) {
            auto copy = t.iterator++;        
            copy->slot(args...);
            if (t.sig == nullptr)
                break;
        }
    }

    ~signal() {
        for (iteration_token* t = top_token; t != nullptr; t = t->next)
            t->sig = nullptr;
        
        while (!connections.empty()) {
            connections.begin()->clean();
        }
    }

    struct iteration_token {
    private:
        typename connections_t::const_iterator iterator;
        signal const *sig = nullptr;
        iteration_token *next = nullptr;    
        friend struct signal;
    
    public:
        iteration_token(const iteration_token &) = delete;
        iteration_token &operator=(const iteration_token &) = delete;
        
        explicit iteration_token(signal const *sig) noexcept: sig(sig), 
                iterator(sig->connections.begin()), next(sig->top_token) {
            sig->top_token = this;
        }

        ~iteration_token() {
            if (sig != nullptr)
                sig->top_token = next;
        }
    };

    struct connection : intrusive::list_element<connection_tag> {
    private:
        signal *sig = nullptr;
        slot_t slot;
        friend struct signal;
    
    public:
        connection() noexcept = default;
        connection(connection const &) = delete;
        connection &operator=(connection const &) = delete;
        
        connection(signal* sig, slot_t slot) noexcept : slot(std::move(slot)), sig(sig) {
            sig->connections.push_back(*this);
        }

        connection(connection &&other) noexcept: slot(std::move(other.slot)), sig(other.sig) {
            move_ctor(other);
        }

        connection& operator=(connection&& other) noexcept {
            if (this != &other) {
                disconnect();
                slot = std::move(other.slot);
                sig = other.sig;
                move_ctor(other);
            }
            return *this;
        }

        ~connection() {
            disconnect();
        }

        void disconnect() noexcept {
            if (sig != nullptr && this->linked()) {
                for (iteration_token *t = sig->top_token; t != nullptr; t = t->next) {
                    if (&*t->iterator == this) {
                        --t->iterator;
                    }
                }
                clean();
            }
        }

    private:
        void clean() noexcept {
            this->unlink();
            slot = slot_t();
            sig = nullptr;
        }

        void move_ctor(connection& other) {
            if (sig != nullptr) {
                sig->connections.insert(sig->connections.as_iterator(other), *this);
                other.unlink();
                for (iteration_token* t = sig->top_token; t != nullptr; t = t->next)
                    if (t->iterator != sig->connections.end() && &(*(t->iterator)) == &other)
                        t->iterator = sig->connections.as_iterator(*this); 
            }
        }
    };

};

}
