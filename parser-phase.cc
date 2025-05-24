#include <unistd.h>
#include <cstdio>
#include <string>
#include "cool-tree.h"
#include "utilities.h"
#include "cool-parse.h"

std::FILE *token_file = stdin;
extern Classes parse_results;
extern Program ast_root;
extern IdTable idtable;
extern IntTable inttable;
extern StrTable stringtable;

extern int curr_lineno;
const char *curr_filename = "<stdin>";
extern int parse_errors;

// Debug flags
extern int yy_flex_debug;
extern int cool_yydebug;
int lex_verbose = 0;

extern int cool_yyparse();

int main(int argc, char **argv)
{
    yy_flex_debug = 0;
    cool_yydebug = 0;
    lex_verbose  = 0;

    for (int i = 1; i < argc; i++) {
        token_file = std::fopen(argv[i], "r");
        if (token_file == NULL) {
            std::cerr << "Error: can not open file " << argv[i] << std::endl;
            std::exit(1);
        }
        curr_filename = argv[i];
        curr_lineno = 1;

        cool_yyparse();
        if (parse_errors != 0) {
            std::cerr << "Error: parse errors\n";
            std::exit(1);
        }
        std::cout << "AST dump with types from file: " << curr_filename << '\n';
        ast_root->dump_with_types(std::cerr, 1);

        std::cout << "Symbol tables:\n";
        std::cout << "# Identifiers:\n";
        idtable.print();
        std::cout << "# Strings:\n";
        stringtable.print();
        std::cout << "# Integers:\n";
        inttable.print();

        std::fclose(token_file);
    }

    return 0;
}