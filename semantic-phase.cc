#include "cool-parse.h"
#include "cool-tree.h"
#include "utilities.h"
#include <cstdio>
#include <functional>
#include <iostream>
#include <unistd.h>
#include <unordered_map>
#include <unordered_set>

std::FILE *token_file = stdin;
extern Classes parse_results;
extern Program ast_root;
extern int curr_lineno;
const char *curr_filename = "<stdin>";
extern int parse_errors;
extern int yy_flex_debug;
extern int cool_yydebug;
int lex_verbose = 0;
extern int cool_yyparse();

using STable = std::unordered_map<std::string, std::string>;
using SSet = std::unordered_set<std::string>;
using FeaturesTable = std::unordered_map<std::string, STable>;

namespace semantic {

int err_count = 0;
void error(std::string error_msg) {
  std::cerr << "Semantic error: " << error_msg << '\n';
  err_count++;
}

void sequence_out(std::string title, SSet set) {
  std::cerr << title << ": ";
  for (auto s : set) {
    std::cerr << s << ' ';
  }
  std::cerr << '\n';
}

// Detects cycles in the inheritance hierarchy
bool detect_cycle(STable hierarchy) {
  SSet visited;
  SSet currentlyVisiting;
  std::function<bool(const std::string &)> dfs =
      [&](const std::string &className) {
        if (visited.find(className) != visited.end()) {
          return false;
        }
        if (currentlyVisiting.find(className) != currentlyVisiting.end()) {
          return true;
        }
        currentlyVisiting.insert(className);
        auto it = hierarchy.find(className);
        if (it != hierarchy.end()) {
          if (dfs(it->second)) {
            return true;
          }
        }
        visited.insert(className);
        currentlyVisiting.erase(className);
        return false;
      };

  for (const auto &entry : hierarchy) {
    if (hierarchy.find(entry.second) == hierarchy.end() &&
        entry.second != "Object") {
      error("parent class '" + entry.second + "' for class '" + entry.first + 
            "' is not defined");
    }
    if (dfs(entry.first)) {
      return true;
    }
  }
  return false;
}

Features getFeatures(tree_node *node) {
  GetFeatures visitor;
  node->accept(visitor);
  return visitor.features;
}

std::string getName(tree_node *node) {
  GetName visitor;
  node->accept(visitor);
  return visitor.name;
}

std::string getParentName(tree_node *node) {
  GetParent visitor;
  node->accept(visitor);
  return std::string(visitor.name);
}

std::string getType(tree_node *node) {
  GetType visitor;
  node->accept(visitor);
  return std::string(visitor.type);
}

Formals getFormals(tree_node *node) {
  GetFormals visitor;
  node->accept(visitor);
  return visitor.formals;
}

Expression getExpression(tree_node *node) {
  GetExpression visitor;
  node->accept(visitor);
  return visitor.expr;
}

Expressions getExpressions(tree_node *node) {
  GetExpressions visitor;
  node->accept(visitor);
  return visitor.exprs;
}

// Checks if two methods have the same signature
bool CheckSignatures(method_class *m1, method_class *m2) {
  if (getType(m1) != getType(m2)) {
    return false;
  }

  Formals m1_formals = getFormals(m1);
  Formals m2_formals = getFormals(m2);

  if (m1_formals->len() != m2_formals->len()) {
    return false;
  }

  for (int i = m1_formals->first(); m1_formals->more(i);
       i = m1_formals->next(i)) {
    formal_class *m1_formal = dynamic_cast<formal_class *>(m1_formals->nth(i));
    formal_class *m2_formal = dynamic_cast<formal_class *>(m2_formals->nth(i));

    if (getName(m1_formal) != getName(m2_formal)) {
      return false;
    }

    if (getType(m1_formal) != getType(m2_formal)) {
      return false;
    }
  }
  return true;
}

class__class *FindClass(std::string name, Classes classes) {
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    class__class *cur_class = dynamic_cast<class__class *>(classes->nth(i));
    if (name == getName(cur_class)) {
      return cur_class;
    }
  }
  return nullptr;
}

void dump_symtables(IdTable idtable, StrTable strtable, IntTable inttable) {
  ast_root->dump_with_types(std::cerr, 0);
  std::cerr << "# Identifiers:\n";
  idtable.print();
  std::cerr << "# Strings:\n";
  stringtable.print();
  std::cerr << "# Integers:\n";
  inttable.print();
}

// Checks if the initialization expression is of the correct type
void check_builtin_types_init(std::string type, Expression expr) {
  std::string expr_type = expr->get_expr_type();
  if (expr_type == "no_expr_class") {
    return;
  }
  if (type == "Int" && expr_type != "int_const_class") {
    error("attempt to initialize Int with non-integer value");
  } else if (type == "Bool" && expr_type != "bool_const_class") {
    error("attempt to initialize Bool with non-boolean value");
  } else if (type == "String" && expr_type != "string_const_class") {
    error("attempt to initialize String with non-string value");
  }
}

bool class_has_method(const std::string& class_name, const std::string& method_name, Classes all_classes) {
    if (class_name == "Int" || class_name == "Bool" || class_name == "String") {
        return false;
    }

    for (int i = all_classes->first(); all_classes->more(i); i = all_classes->next(i)) {
        class__class* cls = dynamic_cast<class__class*>(all_classes->nth(i));
        if (getName(cls) == class_name) {
            Features feats = getFeatures(cls);
            for (int j = feats->first(); feats->more(j); j = feats->next(j)) {
                Feature f = feats->nth(j);
                if (f->get_feature_type() == "method_class") {
                    if (getName(f) == method_name) {
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

std::string get_method_return_type(const std::string& class_name, const std::string& method_name, Classes all_classes) {
    for (int i = all_classes->first(); all_classes->more(i); i = all_classes->next(i)) {
        class__class* cls = dynamic_cast<class__class*>(all_classes->nth(i));
        if (getName(cls) == class_name) {
            Features feats = getFeatures(cls);
            for (int j = feats->first(); feats->more(j); j = feats->next(j)) {
                Feature f = feats->nth(j);
                if (f->get_feature_type() == "method_class") {
                    if (getName(f) == method_name) {
                        return getType(f);
                    }
                }
            }
        }
    }
    return "error";
}

// Checks the type of an expression
void checkExpression(Expression expr, STable &attr_to_type,
                     STable &formal_to_type, SSet &classes_names,
                     SSet &formals_names, FeaturesTable &classes_features) {
  std::string expr_type = expr->get_expr_type();

  if (expr_type == "block_class") {
    Expressions exprs = semantic::getExpressions(expr);
    for (int l = exprs->first(); exprs->more(l); l = exprs->next(l)) {
      checkExpression(exprs->nth(l), attr_to_type, formal_to_type,
                      classes_names, formals_names, classes_features);
    }

  } else if (expr_type == "let_class") {
    std::string formal_name = semantic::getName(expr);

    if (formal_name == "self") {
      semantic::error("can't use 'self' as new local variable name");
    }

    auto result = formals_names.insert(formal_name);
    if (!result.second) {
      semantic::error("formal '" + formal_name + "' already defined in scope");
    }

    std::string expr_type = semantic::getType(expr);
    if (classes_names.find(expr_type) == classes_names.end()) {
      semantic::error("unknown type '" + expr_type + "' in " + formal_name);
    }

  } else if (expr_type == "plus_class" || expr_type == "sub_class" ||
             expr_type == "mul_class" || expr_type == "divide_class") {
    Expressions exprs = semantic::getExpressions(expr);
    for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
      Expression e = exprs->nth(i);

      if (e->get_expr_type() == "object_class") {
        std::string var_name = getName(e);
        if (var_name != "self" && 
            attr_to_type.find(var_name) == attr_to_type.end() && 
            formal_to_type.find(var_name) == formal_to_type.end()) {
          error("variable '" + var_name + "' not defined in scope");
          return;
        }
      }

      bool int_const_check = e->get_expr_type() == "int_const_class" || e->get_expr_type() == "plus_class" || e->get_expr_type() == "sub_class" || e->get_expr_type() == "mul_class" || e->get_expr_type() == "divide_class";
      bool static_dispatch_check =
          e->get_expr_type() == "static_dispatch_class" && getType(e) == "Int";

      bool dispatch_check = e->get_expr_type() == "dispatch_class";
      for (auto &[_, map] : classes_features) {
        if (map[getName(e)] == "Int") {
          dispatch_check &= true;
        }
      }

      bool object_check = e->get_expr_type() == "object_class" &&
                          (attr_to_type[getName(e)] == "Int" ||
                           formal_to_type[getName(e)] == "Int");
      if (!int_const_check && !static_dispatch_check && !dispatch_check && !object_check) {
        error("non-integer value " + e->get_expr_type() + " in arithmetic operation");
      }
    }
  } else if (expr_type == "dispatch_class") {
    Expression e = getExpression(expr);
    if (!e) {
      error("dispatch: calling expression is nullptr");
      return;
    }
    std::string method_name = getName(expr);
    std::string caller_type;
    if (e->get_expr_type() == "int_const_class") {
        caller_type = "Int";
        error("Class 'Int' has no method '" + method_name + "'");
        return;
    } else if (e->get_expr_type() == "bool_const_class") {
        caller_type = "Bool";
        error("Class 'Bool' has no method '" + method_name + "'");
        return;
    } else if (e->get_expr_type() == "string_const_class") {
        caller_type = "String";
        error("Class 'String' has no method '" + method_name + "'");
        return;
    } else if (e->get_expr_type() == "object_class") {
        std::string var_name = getName(e);
        if (var_name == "self") {
            caller_type = "self";
        } else if (attr_to_type.find(var_name) != attr_to_type.end()) {
            caller_type = attr_to_type[var_name];
        } else if (formal_to_type.find(var_name) != formal_to_type.end()) {
            caller_type = formal_to_type[var_name];
        } else {
            error("variable '" + var_name + "' not defined in scope");
            return;
        }
    } else {
        checkExpression(e, attr_to_type, formal_to_type, classes_names, formals_names, classes_features);
        if (e->get_expr_type() == "int_const_class") {
            caller_type = "Int";
            error("Class 'Int' has no method '" + method_name + "'");
            return;
        } else if (e->get_expr_type() == "bool_const_class") {
            caller_type = "Bool";
            error("Class 'Bool' has no method '" + method_name + "'");
            return;
        } else if (e->get_expr_type() == "string_const_class") {
            caller_type = "String";
            error("Class 'String' has no method '" + method_name + "'");
            return;
        } else if (e->get_expr_type() == "object_class") {
            std::string var_name = getName(e);
            if (var_name == "self") {
                caller_type = "self";
            } else if (attr_to_type.find(var_name) != attr_to_type.end()) {
                caller_type = attr_to_type[var_name];
            } else if (formal_to_type.find(var_name) != formal_to_type.end()) {
                caller_type = formal_to_type[var_name];
            } else {
                caller_type = "Object";
            }
        } else {
            caller_type = "Object";
        }
    }
    bool method_found = false;
    std::string current_type = caller_type;
    while (!method_found && current_type != "Object") {
        if (class_has_method(current_type, method_name, parse_results)) {
            method_found = true;
            break;
        }
        class__class* current_class = FindClass(current_type, parse_results);
        if (current_class) {
            current_type = getParentName(current_class);
        } else {
            break;
        }
    }
    if (!method_found) {
        error("Class '" + caller_type + "' has no method '" + method_name + "'");
        return;
    }
    Expressions actuals = getExpressions(expr);
    if (!actuals) {
      return;
    } else {
      for (int i = actuals->first(); actuals->more(i); i = actuals->next(i)) {
        if (!actuals->nth(i)) {
          return;
        } else {
          checkExpression(actuals->nth(i), attr_to_type, formal_to_type, classes_names, formals_names, classes_features);
        }
      }
    }
  } else if (expr_type == "neg_class") {
    Expression e = getExpression(expr);
    bool bool_const_check = e->get_expr_type() == "bool_const_class" || e->get_expr_type() == "lt_class" || e->get_expr_type() == "eq_class" || e->get_expr_type() == "leq_class";
    bool static_dispatch_check =
            e->get_expr_type() == "static_dispatch_class" && getType(e) == "Bool";

    bool dispatch_check = e->get_expr_type() == "dispatch_class";
    for (auto &[_, map] : classes_features) {
      if (map[getName(e)] == "Bool") {
        dispatch_check &= true;
      }
    }

    bool object_check = e->get_expr_type() == "object_class" &&
                        (attr_to_type[getName(e)] == "Bool" ||
                         formal_to_type[getName(e)] == "Bool");
    if (!bool_const_check && !static_dispatch_check && !dispatch_check && !object_check) {
      error("non-boolean value " + e->get_expr_type() + " in negative (~) operation");
    }

  } else if (expr_type == "lt_class" || expr_type == "leq_class") {
    Expressions exprs = getExpressions(expr);
    for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
      Expression e = exprs->nth(i);
      bool int_const_check = e->get_expr_type() == "int_const_class";
      bool static_dispatch_check =
              e->get_expr_type() == "static_dispatch_class" && getType(e) == "Int";

      bool dispatch_check = e->get_expr_type() == "dispatch_class";
      for (auto &[_, map] : classes_features) {
        if (map[getName(e)] == "Int") {
          dispatch_check &= true;
        }
      }

      bool object_check = e->get_expr_type() == "object_class" &&
                          (attr_to_type[getName(e)] == "Int" ||
                           formal_to_type[getName(e)] == "Int");
      if (!int_const_check && !static_dispatch_check && !dispatch_check && !object_check) {
        error("non-integer value " + e->get_expr_type() + " in less-based compare operation");
      }
    }

  } else if (expr_type == "eq_class") {
    Expressions exprs = getExpressions(expr);
    for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
      Expression e = exprs->nth(i);
      bool const_check = e->get_expr_type() == "int_const_class" || e->get_expr_type() == "bool_const_class" || e->get_expr_type() == "lt_class" || e->get_expr_type() == "eq_class" || e->get_expr_type() == "leq_class" || e->get_expr_type() == "plus_class" || e->get_expr_type() == "sub_class" || e->get_expr_type() == "mul_class" || e->get_expr_type() == "divide_class";
      bool static_dispatch_check =
              e->get_expr_type() == "static_dispatch_class" && (getType(e) == "Int" || getType(e) == "Bool");

      bool dispatch_check = e->get_expr_type() == "dispatch_class";
      for (auto &[_, map] : classes_features) {
        if (map[getName(e)] == "Int" || map[getName(e)] == "Bool") {
          dispatch_check &= true;
        }
      }

      bool object_check = e->get_expr_type() == "object_class" &&
                          (attr_to_type[getName(e)] == "Int" ||
                           formal_to_type[getName(e)] == "Int" || attr_to_type[getName(e)] == "Bool" || formal_to_type[getName(e)] == "Bool");
      if (!const_check && !static_dispatch_check && !dispatch_check && !object_check) {
        error("non-Int or non-Bool value " + e->get_expr_type() + " in equal (=) operation");
      }
    }

  } else if (expr_type == "cond_class") {
    Expression e = getExpression(expr);
    bool bool_const_check = e->get_expr_type() == "bool_const_class" || e->get_expr_type() == "lt_class" || e->get_expr_type() == "eq_class" || e->get_expr_type() == "leq_class";
    bool static_dispatch_check =
            e->get_expr_type() == "static_dispatch_class" && getType(e) == "Bool";

    bool dispatch_check = e->get_expr_type() == "dispatch_class";
    for (auto &[_, map] : classes_features) {
      if (map[getName(e)] == "Bool") {
        dispatch_check &= true;
      }
    }

    bool object_check = e->get_expr_type() == "object_class" &&
                        (attr_to_type[getName(e)] == "Bool" ||
                         formal_to_type[getName(e)] == "Bool");
    if (!bool_const_check && !static_dispatch_check && !dispatch_check && !object_check) {
      error("non-boolean value " + e->get_expr_type() + " in if-condition");
    }

  } 
}
};

int main(int argc, char **argv) {
  yy_flex_debug = 0;
  cool_yydebug = 0;
  lex_verbose = 0;
  for (int i = 1; i < argc; i++) {
    token_file = std::fopen(argv[i], "r");
    if (token_file == NULL) {
      std::cerr << "Error: failed attempt to open file " << argv[i] << std::endl;
      std::exit(1);
    }
    curr_lineno = 1;
    cool_yyparse();
    if (parse_errors != 0) {
      std::cerr << "Error: parse errors in file " << argv[i] << std::endl;
      std::exit(1);
    }
    //semantic::dump_symtables(idtable, stringtable, inttable);

    FeaturesTable classes_features;
    STable classes_hierarchy;
    SSet non_inherited{"Bool", "Int", "String", "SELF_TYPE"};
    SSet classes_names(non_inherited);
    classes_names.insert("Object");

    // Check class uniqueness and inheritance hierarchy
    for (int i = parse_results->first(); parse_results->more(i);
         i = parse_results->next(i)) {
      class__class *current_class =
          dynamic_cast<class__class *>(parse_results->nth(i));
      std::string class_name = semantic::getName(current_class);

      // Check class name uniqueness
      auto result = classes_names.insert(class_name);
      if (!result.second) {
        semantic::error("class '" + std::string(class_name) +
                        "' already defined in scope");
      }

      std::string parent_name = semantic::getParentName(current_class);
      classes_hierarchy[class_name] = parent_name;

      // Check parent class validity
      if (non_inherited.find(parent_name) != non_inherited.end()) {
        semantic::error("failed to use parent class '" + parent_name + "' for class '" +
                        class_name + "' (builtin)");
      }

      Features features = semantic::getFeatures(current_class);
      SSet features_names;
      STable features_types;
      STable attr_to_type;

      // Check feature uniqueness and types
      for (int j = features->first(); features->more(j);
           j = features->next(j)) {

        Feature current_feature = features->nth(j);
        std::string feature_name = semantic::getName(current_feature);

        if (feature_name == "self") {
          semantic::error("failed to use 'self' as feature name");
        }

        // Check feature name uniqueness
        result = features_names.insert(feature_name);
        if (!result.second) {
          semantic::error("feature '" + std::string(feature_name) + "' in '" +
                          class_name + "' already defined in scope");
        }

        std::string feature_type = semantic::getType(current_feature);

        // Check type existence
        if (classes_names.find(feature_type) == classes_names.end()) {
          semantic::error("unknown type '" + feature_type + "' in " +
                          feature_name);
        }

        if (feature_type == "SELF_TYPE") {
          semantic::error("failed to use SELF_TYPE as a type inside class");
        }
        features_types[feature_name] = feature_type;

        if (current_feature->get_feature_type() == "method_class") {
          Formals formals = semantic::getFormals(current_feature);

          // Check method inheritance and overrides
          if (std::string(parent_name) != "Object") {
            class__class *parent =
                semantic::FindClass(parent_name, parse_results);

            if (parent) {
              Features parent_features = semantic::getFeatures(parent);

              // Check method overrides
              for (int a = parent_features->first(); parent_features->more(a);
                   a = parent_features->next(a)) {
                Feature parent_feature = parent_features->nth(a);
                std::string parent_feature_name =
                    semantic::getName(parent_feature);

                if (parent_feature_name == feature_name) {
                  // Check feature type match
                  if (parent_feature->get_feature_type() !=
                      current_feature->get_feature_type()) {
                    semantic::error("failed to override feature '" +
                                    feature_name + "' from class '" +
                                    parent_name + "' in class '" + class_name +
                                    "'");
                  }

                  // Check method signatures
                  method_class *cur_method =
                      dynamic_cast<method_class *>(current_feature);
                  method_class *parent_method =
                      dynamic_cast<method_class *>(parent_feature);
                  if (!semantic::CheckSignatures(cur_method, parent_method)) {
                    semantic::error(
                        "'" + feature_name + "' method from class '" +
                        parent_name +
                        "' failed to match override version of it in class '" +
                        class_name + "'");
                  }
                }
              }
            } else {
              semantic::error("parent class '" + parent_name + "' of class '" +
                              class_name + "' is not defined");
            }
          }

          STable formal_to_type;
          SSet formals_names;

          // Check formal parameters
          for (int k = formals->first(); formals->more(k);
               k = formals->next(k)) {
            Formal_class *current_formal =
                dynamic_cast<formal_class *>(formals->nth(k));
            std::string formal_name = semantic::getName(current_formal);

            if (formal_name == "self") {
              semantic::error("failed to use 'self' as formal name");
            }

            // Check formal name uniqueness
            result = formals_names.insert(formal_name);
            if (!result.second) {
              semantic::error("formal '" + std::string(formal_name) + "' in '" +
                              feature_name + "' already defined in scope");
            }

            // Check formal type
            std::string formal_type = semantic::getType(current_formal);
            if (classes_names.find(formal_type) == classes_names.end()) {
              semantic::error("unknown type '" + formal_type + "' in " +
                              formal_name);
            }

            formal_to_type[formal_name] = formal_type;
          }

          // Check expression types
          Expression expr = semantic::getExpression(current_feature);
          semantic::checkExpression(expr, attr_to_type, formal_to_type,
                                    classes_names, formals_names, classes_features);

        } else {
          // Check attribute initialization
          attr_class *attr = dynamic_cast<attr_class *>(current_feature);
          std::string attr_name = semantic::getName(attr);
          std::string attr_type = semantic::getType(attr);
          semantic::check_builtin_types_init(attr_type,
                                             semantic::getExpression(attr));
          attr_to_type[attr_name] = attr_type;
        }
      }

      classes_features[class_name] = features_types;
    }

    // Check if there is at least one class with main method
    bool has_main_method = false;
    for (int i = parse_results->first(); parse_results->more(i); i = parse_results->next(i)) {
      class__class *current_class = dynamic_cast<class__class *>(parse_results->nth(i));
      Features features = semantic::getFeatures(current_class);
      for (int j = features->first(); features->more(j); j = features->next(j)) {
        Feature current_feature = features->nth(j);
        if (current_feature->get_feature_type() == "method_class" && 
            semantic::getName(current_feature) == "main") {
          has_main_method = true;
          break;
        }
      }
      if (has_main_method) break;
    }
    if (!has_main_method) {
      semantic::error("no class with method 'main' found");
    }

    // Check inheritance hierarchy for cycles
    if (semantic::detect_cycle(classes_hierarchy)) {
      semantic::error("loop detected in classes inheritance hierarchy");
      for (auto p : classes_hierarchy) {
        std::cerr << '\t' << p.first << " : " << p.second << "\n";
      }
    }
    std::fclose(token_file);
  }
  std::cerr << "Semantic phase: " << semantic::err_count << " errors\n";
  return semantic::err_count;
}