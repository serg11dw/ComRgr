#ifndef TREE_H
#define TREE_H

#include <iostream>
#include "stringtab.h"

extern int node_lineno;
const char *pad(int n);

template <class Elem> class list_node;
template <class Elem> class nil_node;
template <class Elem> class single_list_node;
template <class Elem> class append_node;

class class__class;
class method_class;
class attr_class;
class formal_class;
class branch_class;
class assign_class;
class static_dispatch_class;
class dispatch_class;
class let_class;
class new__class;
class object_class;
class block_class;
class plus_class;
class sub_class;
class mul_class;
class divide_class;
class string_const_class;
class bool_const_class;
class int_const_class;
class neg_class;
class lt_class;
class eq_class;
class leq_class;
class cond_class;

class Visitor {
public:
  virtual void visit(class__class &ref) {}
  virtual void visit(method_class &ref) {}
  virtual void visit(attr_class &ref) {}
  virtual void visit(formal_class &ref) {}
  virtual void visit(branch_class &ref) {}
  virtual void visit(assign_class &ref) {}
  virtual void visit(static_dispatch_class &ref) {}
  virtual void visit(dispatch_class &ref) {}
  virtual void visit(let_class &ref) {}
  virtual void visit(new__class &ref) {}
  virtual void visit(object_class &ref) {}
  virtual void visit(block_class &ref) {}
  virtual void visit(plus_class &ref) {}
  virtual void visit(sub_class &ref) {}
  virtual void visit(mul_class &ref) {}
  virtual void visit(divide_class &ref) {}
  virtual void visit(string_const_class &ref) {}
  virtual void visit(bool_const_class &ref) {}
  virtual void visit(int_const_class &ref) {}
  virtual void visit(neg_class &ref) {}
  virtual void visit(lt_class &ref) {}
  virtual void visit(eq_class &ref) {}
  virtual void visit(leq_class &ref) {}
  virtual void visit(cond_class &ref) {}
  virtual ~Visitor() = default;
};

class tree_node {
protected:
    // line number when node is created
    int line_number;
public:
    tree_node() { line_number = node_lineno; }
    virtual tree_node *copy() = 0;
    virtual ~tree_node() { }
    virtual void dump(std::ostream &stream, int n) = 0;
    int get_line_number() {	return line_number; }
    tree_node *set(tree_node *t) { line_number = t->line_number; return this; }
    virtual void accept(Visitor &v) {}
};

// Lists of objects
template <class Elem>
class list_node : public tree_node {
public:
    tree_node *copy() { return copy_list(); }
    Elem nth(int n) {
        int len;
        Elem tmp = nth_length(n ,len);
        if (tmp)
    	    return tmp;
        else {
	        std::cerr << "error: outside the range of the list\n";
	        std::exit(1);
        }
    }

    // Iterator
    int first() { return 0; }
    int next(int n) { return n + 1; }
    int more(int n) { return n < len(); }

    virtual list_node<Elem> *copy_list() = 0;
    virtual ~list_node() { }
    virtual int len() = 0;

    // Returns the nth element of the list or NULL if there are not n elements.
    // len is set to the length of the list.
    virtual Elem nth_length(int n, int &len) = 0;

    // Construct an empty list
    static list_node<Elem> *nil() { return new nil_node<Elem>(); }
    // List of one element
    static list_node<Elem> *single(Elem e) { return new single_list_node<Elem>(e); }
    // Append two lists
    static list_node<Elem> *append(list_node<Elem> *l1,list_node<Elem> *l2) { return new append_node<Elem>(l1, l2); }
};

template <class Elem>
class nil_node : public list_node<Elem> {
public:
    list_node<Elem> *copy_list() { return new nil_node<Elem>(); }
    int len() { return 0; };
    Elem nth_length(int n, int &len) { len = 0; return NULL; }
    void dump(std::ostream &stream, int n) { stream << pad(n) << "(nil)\n"; }
};

template <class Elem>
class single_list_node : public list_node<Elem> {
private:
    Elem elem;
public:
    single_list_node(Elem t) : elem(t) { }
    list_node<Elem> *copy_list() { return new single_list_node<Elem>((Elem)elem->copy()); }
    int len() { return 1; };
    Elem nth_length(int n, int &len) { len = 1; return (n ? NULL : elem); };
    void dump(std::ostream &  stream, int n) { elem->dump(stream, n); };
};

template <class Elem>
class append_node : public list_node<Elem> {
private:
    list_node<Elem> *some, *rest;
public:
    append_node(list_node<Elem> *l1, list_node<Elem> *l2) { some = l1; rest = l2; }
    list_node<Elem> *copy_list() { return new append_node<Elem>(some->copy_list(), rest->copy_list()); }
    int len() { return some->len() + rest->len(); }
    Elem nth(int n) {
        int len;
        Elem tmp = nth_length(n ,len);
        if (tmp)
    	    return tmp;
        else {
	        std::cerr << "error: outside the range of the list\n";
	        std::exit(1);
        }
    }
    Elem nth_length(int n, int &len) {
        int rlen;
        Elem tmp = some->nth_length(n, len);
        if (!tmp) {
	        tmp = rest->nth_length(n - len, rlen);
	        len += rlen;
        }
        return tmp;
    }
    void dump(std::ostream &  stream, int n) {
        int size = len();
        stream << pad(n) << "list\n";
        for (int i = 0; i < size; i++)
            nth(i)->dump(stream, n + 2);
        stream << pad(n) << "(end_of_list)\n";
    }
};

template <class Elem> single_list_node<Elem> *list(Elem x);
template <class Elem> append_node<Elem> *cons(Elem x, list_node<Elem> *l);
template <class Elem> append_node<Elem> *xcons(list_node<Elem> *l, Elem x);

template <class Elem> single_list_node<Elem> *list(Elem x)
{
    return new single_list_node<Elem>(x);
}

template <class Elem> append_node<Elem> *cons(Elem x, list_node<Elem> *l)
{
    return new append_node<Elem>(list(x), l);
}

template <class Elem> append_node<Elem> *xcons(list_node<Elem> *l, Elem x)
{
    return new append_node<Elem>(l, list(x));
}

#endif