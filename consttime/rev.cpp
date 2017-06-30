#include <stdio.h>
#include <stddef.h>
#include <stdint.h>

template<typename T>
struct node {
    intptr_t delta;
    T data;

    node* sib(node* sibling)
    {
	return (node*)((intptr_t)sibling ^ delta);
    }
    
    static intptr_t diff(node* a, node* b)
    {
	return (intptr_t)a ^ (intptr_t)b;
    }

    friend node* between(node* a, node* b)
    {
	node* tmp = new node;
	tmp->attach(a,b);
	return tmp;
    }

    void attach(node* a, node* b)
    {
	a->delta = diff(a->sib(b), this);
	b->delta = diff(b->sib(a), this);
	delta    = diff(a,b);
    }

    void detach(node* a)
    {
	node* b = sib(a);
	a->delta = diff(a->sib(this), b);
	b->delta = diff(b->sib(this), a);
    }
};

template<typename T>
struct list {
    node<T>* head;
    node<T>* next;
public:
    typedef list iterator;

    list() : head(0) { }

    void push_front(const T& value)
    {
	if(head) {
	    node<T>* prev = head->sib(next);
	    next = head;
	    head = between(prev, next);
	} else {
	    next = head = new node<T>;
	    head->delta = 0;
	}
	head->data = value;
    }

    void push_back(const T& value)
    {
	if(head) {
	    node<T>* prev = head->sib(next);
	    between(prev, head)->data = value;
	} else push_front(value);
    }

    bool empty()
    {
    	return !head;
    }

    list& reverse()
    {
	if(head) {
	    next = head->sib(next);
	    ++*this;
	}
	return *this;
    }

    list& operator++()
    {
	node<T>* prev = head;
	head = next;
	next = head->sib(prev);
	return *this;
    }

    list& operator--()
    {
	node<T>* tmp = next;
	next = head;
	head = head->sib(tmp);
	return *this;
    }

    size_t size()
    {
	size_t acc = 0;
	if(!empty()) {
	    iterator p = begin();
	    do {
	       	++acc;
	    } while(++p != begin());
	}
	return acc;
    }

    // this is just boilerplate
    T& operator*()
    { return head->data; }

    iterator begin()
    { return *this; }

    iterator end()
    { return *this; }

    list operator++(int)
    { list tmp(*this); ++*this; return tmp; }
    list operator--(int)
    { list tmp(*this); --*this; return tmp; }

    friend bool operator==(const list& x, const list& y)
    { return x.head == y.head; }
    friend bool operator!=(const list& x, const list& y)
    { return x.head != y.head; }

};

using namespace std;

int main()
{
    list<int> foo;
    foo.push_front(10);
    foo.push_front(20);
    foo.push_front(30);
    foo.push_front(40);
    foo.push_back(7);
    foo.push_back(8);
    foo.push_back(9);

    list<int>::iterator p = foo.begin();
    if(!foo.empty()) do {
        printf("%d\n", *p++);
    } while(p != foo.end());
    
    printf("---\n");

    foo.reverse();

    p = foo.begin();
    for(int n = foo.size(); n--; ) {
        printf("%d\n", *p++);
    }
    
    return 1;
}
