#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/*
For the language grammar, please refer to Grammar section on the github page:
  https://github.com/lightbulb12294/CSI2P-II-Mini1#grammar
*/

#define MAX_LENGTH 200
typedef enum {
	ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS, END
} Type;
typedef enum {
	STMT, EXPR, ASSIGN_EXPR, ADD_EXPR, MUL_EXPR, UNARY_EXPR, POSTFIX_EXPR, PRI_EXPR
} GrammarState;
typedef struct TokenUnit {
	Type type;
	int val; // record the integer value or variable name
	struct TokenUnit *next;
} Token;
typedef struct ASTUnit {
	Type type;
	int val; // record the integer value or variable name
	struct ASTUnit *lhs, *mid, *rhs;
} AST;

/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x) {\
	puts("Compile Error!");\
	if(DEBUG) {\
		fprintf(stderr, "Error at line: %d\n", __LINE__);\
		fprintf(stderr, "Error message: %s\n", x);\
	}\
	exit(0);\
}
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
#define DEBUG 1
// Split the input char array into token linked list.
Token *lexer(const char *in);
// Create a new token.
Token *new_token(Type type, int val);
// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token **head);
// Parse the token array. Return the constructed AST.
AST *parser(Token *token_array, size_t length);
// Parse the token array. Return the constructed AST.
AST *parse(Token *token_array, int start, int end, GrammarState state);
// Create a new AST node.
AST *new_AST(Type type, int val);
// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token *token_array, int start, int end, int (*cond)(Type));
// Return 1 if type is ASSIGN.
int condASSIGN(Type type);
// Return 1 if type is ADD or SUB.
int condADD(Type type);
// Return 1 if type is MUL, DIV, or REM.
int condMUL(Type type);
// Return 1 if type is RPAR.
int condRPAR(Type type);
// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST *current);
// Generate ASM code.
void codegen(AST *root);
// Free the whole AST.
void freeAST(AST *current);

/// debug interfaces

// Print token array.
void token_print(Token *in, size_t len);
// Print AST tree.
void AST_print(AST *head);

char input[MAX_LENGTH];

int main() {
	while (fgets(input, MAX_LENGTH, stdin) != NULL) {
		Token *content = lexer(input);
		size_t len = token_list_to_arr(&content);
		if (len == 0) continue;
		AST *ast_root = parser(content, len);
		semantic_check(ast_root);
		codegen(ast_root);
		free(content);
		freeAST(ast_root);
	}
	return 0;
}

Token *lexer(const char *in) {
	Token *head = NULL;
	Token **current = &head;
	for (int i = 0; in[i]; i++) {
		if (isspace(in[i])) // ignore space characters
			continue;
		else if (isdigit(in[i])) {
			(*current) = new_token(CONSTANT, atoi(in + i));
			while (in[i+1] && isdigit(in[i+1])) i++;
		}
		else if ('x' <= in[i] && in[i] <= 'z') // variable
			(*current) = new_token(IDENTIFIER, in[i]);
		else switch (in[i]) {
			case '=':
				(*current) = new_token(ASSIGN, 0);
				break;
			case '+':
				if (in[i+1] && in[i+1] == '+') {
					i++;
					// In lexer scope, all "++" will be labeled as PREINC.
					(*current) = new_token(PREINC, 0);
				}
				// In lexer scope, all single "+" will be labeled as PLUS.
				else (*current) = new_token(PLUS, 0);
				break;
			case '-':
				if (in[i+1] && in[i+1] == '-') {
					i++;
					// In lexer scope, all "--" will be labeled as PREDEC.
					(*current) = new_token(PREDEC, 0);
				}
				// In lexer scope, all single "-" will be labeled as MINUS.
				else (*current) = new_token(MINUS, 0);
				break;
			case '*':
				(*current) = new_token(MUL, 0);
				break;
			case '/':
				(*current) = new_token(DIV, 0);
				break;
			case '%':
				(*current) = new_token(REM, 0);
				break;
			case '(':
				(*current) = new_token(LPAR, 0);
				break;
			case ')':
				(*current) = new_token(RPAR, 0);
				break;
			case ';':
				(*current) = new_token(END, 0);
				break;
			default:
				err("Unexpected character.");
		}
		current = &((*current)->next);
	}
	return head;
}

Token *new_token(Type type, int val) {
	Token *res = (Token*)malloc(sizeof(Token));
	res->type = type;
	res->val = val;
	res->next = NULL;
	return res;
}

size_t token_list_to_arr(Token **head) {
	size_t res;
	Token *current = (*head), *del;
	for (res = 0; current != NULL; res++) current = current->next;
	current = (*head);
	if (res != 0) (*head) = (Token*)malloc(sizeof(Token) * res);
	for (int i = 0; i < res; i++) {
		(*head)[i] = (*current);
		del = current;
		current = current->next;
		free(del);
	}
	return res;
}

AST *parser(Token *token_array, size_t length) {
	for (int i = 1; i < length; i++) {
		// correctly identify "ADD" and "SUB"
		if (token_array[i].type == PLUS || token_array[i].type == MINUS) {
			switch (token_array[i - 1].type) {
				case PREINC:
				case PREDEC:
				case IDENTIFIER:
				case CONSTANT:
				case RPAR:
					token_array[i].type = token_array[i].type - PLUS + ADD;
				default: break;
			}
		}
	}
	return parse(token_array, 0, length - 1, STMT);
}

AST *parse(Token *token_array, int start, int end, GrammarState state) {
	AST *current = NULL;
	if (start > end)
		err("Unexpected parsing range.");
	int next;
	switch (state) {
		case STMT:
			if (start == end && token_array[start].type == END)
				return NULL;
			else if (token_array[end].type == END)
				return parse(token_array, start, end - 1, EXPR);
			else err("Expected \';\' at the end of line.");
		case EXPR:
			return parse(token_array, start, end, ASSIGN_EXPR);
		case ASSIGN_EXPR:
			if ((next = findNextSection(token_array, start, end, condASSIGN)) != -1) {
				current = new_AST(token_array[next].type, 0);
				current->lhs = parse(token_array, start, next - 1, UNARY_EXPR);
				current->rhs = parse(token_array, next + 1, end, ASSIGN_EXPR);
				return current;
			}
			return parse(token_array, start, end, ADD_EXPR);
		case ADD_EXPR:
			if((next = findNextSection(token_array, end, start, condADD)) != -1) {
				current = new_AST(token_array[next].type, 0);
				current->lhs = parse(token_array, start, next - 1, ADD_EXPR);
				current->rhs = parse(token_array, next + 1, end, MUL_EXPR);
				return current;
			}
			return parse(token_array, start, end, MUL_EXPR);
		case MUL_EXPR:
			if((next = findNextSection(token_array, end, start, condMUL)) != -1) {
				current = new_AST(token_array[next].type, 0);
				current->lhs = parse(token_array, start, next - 1, MUL_EXPR);
				current->rhs = parse(token_array, next + 1, end, UNARY_EXPR);
				return current;
			}
			return parse(token_array, start, end, UNARY_EXPR);
		case UNARY_EXPR:
			if (token_array[start].type == PREINC || token_array[start].type == PREDEC || token_array[start].type == PLUS || token_array[start].type == MINUS) {
				current = new_AST(token_array[start].type, 0);
				current->mid = parse(token_array, start + 1, end, UNARY_EXPR);
				return current;
			}
			return parse(token_array, start, end, POSTFIX_EXPR);
		case POSTFIX_EXPR:
			if (token_array[end].type == PREINC || token_array[end].type == PREDEC) {
				// translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
				current = new_AST(token_array[end].type - PREINC + POSTINC, 0);
				current->mid = parse(token_array, start, end - 1, POSTFIX_EXPR);
				return current;
			}
			return parse(token_array, start, end, PRI_EXPR);
		case PRI_EXPR:
			if (findNextSection(token_array, start, end, condRPAR) == end) {
				current = new_AST(LPAR, 0);
				current->mid = parse(token_array, start + 1, end - 1, EXPR);
				return current;
			}
			if (start == end) {
				if (token_array[start].type == IDENTIFIER || token_array[start].type == CONSTANT)
					return new_AST(token_array[start].type, token_array[start].val);
				err("Unexpected token during parsing.");
			}
			err("No token left for parsing.");
		default:
			err("Unexpected grammar state.");
	}
}

AST *new_AST(Type type, int val) {
	AST *res = (AST*)malloc(sizeof(AST));
	res->type = type;
	res->val = val;
	res->lhs = res->mid = res->rhs = NULL;
	return res;
}

int findNextSection(Token *arr, int start, int end, int (*cond)(Type)) {
	int par = 0;
	int d = (start < end) ? 1 : -1;
	for (int i = start; (start < end) ? (i <= end) : (i >= end); i += d) {
		if (arr[i].type == LPAR) par++;
		if (arr[i].type == RPAR) par--;
		if (par == 0 && cond(arr[i].type) == 1) return i;
	}
	return -1;
}

int condASSIGN(Type type) {
	return type == ASSIGN;
}

int condADD(Type type) {
	return type == ADD || type == SUB;
}

int condMUL(Type type) {
	return type == MUL || type == DIV || type == REM;
}

int condRPAR(Type type) {
	return type == RPAR;
}

void semantic_check(AST *current) {
	if (current == NULL) return;
	// Left operand of '=' must be an identifier or identifier with one or more parentheses.
	if (current->type == ASSIGN) {
		AST *lhs = current->lhs;
		while (lhs->type == LPAR) lhs = lhs->mid;
		if (lhs->type != IDENTIFIER)
			err("Lvalue is required as left operand of assignment.");
	}
	// Operand of INC/DEC must be an identifier or identifier with one or more parentheses.
	else if (current->type == PREINC || current->type == PREDEC || current->type == POSTINC || current->type == POSTDEC) {
		AST* lhs = current->lhs;
		while (lhs->type == LPAR) lhs = lhs->mid;
		if (lhs->type != IDENTIFIER)
			err("Lvalue is required as operand of increment / decrement.")
	}
	else {
		semantic_check(current->lhs);
		semantic_check(current->mid);
		semantic_check(current->rhs);
	}
}

void codegen(AST *root) {
	// TODO: Implement your codegen in your own way.
	// You may modify the function parameter or the return type, even the whole structure as you wish.
}

void freeAST(AST *current) {
	if (current == NULL) return;
	freeAST(current->lhs);
	freeAST(current->mid);
	freeAST(current->rhs);
	free(current);
}

void token_print(Token *in, size_t len) {
	const static char typeName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "Inc", "Dec", "Inc", "Dec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus", "End"
	};
	const static char typeSymbol[][20] = {
		"'='", "'+'", "'-'", "'*'", "'/'", "'%'", "\"++\"", "\"--\"", "\"++\"", "\"--\"", "", "", "'('", "')'", "'+'", "'-'"
	};
	const static char format_str[] = "<Index = %3d>: %-10s, %-6s = %s\n";
	const static char format_int[] = "<Index = %3d>: %-10s, %-6s = %d\n";
	for(int i = 0; i < len; i++) {
		switch(in[i].type) {
			case LPAR:
			case RPAR:
			case PREINC:
			case PREDEC:
			case ADD:
			case SUB:
			case MUL:
			case DIV:
			case REM:
			case ASSIGN:
			case PLUS:
			case MINUS:
				fprintf(stderr, format_str, i, typeName[in[i].type], "symbol", typeSymbol[in[i].type]);
				break;
			case CONSTANT:
				fprintf(stderr, format_int, i, typeName[in[i].type], "value", in[i].val);
				break;
			case IDENTIFIER:
				fprintf(stderr, format_str, i, typeName[in[i].type], "name", (char*)(&(in[i].val)));
				break;
			case END:
				fprintf(stderr, "<Index = %3d>: %-10s\n", i, typeName[in[i].type]);
				break;
			default:
				fputs("=== unknown token ===", stderr);
		}
	}
}

void AST_print(AST *head) {
	static char indent_str[MAX_LENGTH] = "  ";
	static int indent = 2;
	const static char typeName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"
	};
	const static char format[] = "%s\n";
	const static char format_str[] = "%s, <%s = %s>\n";
	const static char format_val[] = "%s, <%s = %d>\n";
	if (head == NULL) return;
	char *indent_current = indent_str + indent;
	indent_str[indent - 1] = '-';
	fprintf(stderr, "%s", indent_str);
	indent_str[indent - 1] = ' ';
	if (indent_str[indent - 2] == '`')
		indent_str[indent - 2] = ' ';
	switch (head->type) {
		case ASSIGN:
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case REM:
		case PREINC:
		case PREDEC:
		case POSTINC:
		case POSTDEC:
		case LPAR:
		case RPAR:
		case PLUS:
		case MINUS:
			fprintf(stderr, format, typeName[head->type]);
			break;
		case IDENTIFIER:
			fprintf(stderr, format_str, typeName[head->type], "name", (char*)&(head->val));
			break;
		case CONSTANT:
			fprintf(stderr, format_val, typeName[head->type], "value", head->val);
			break;
		default:
			fputs("=== unknown AST type ===", stderr);
	}
	indent += 2;
	strcpy(indent_current, "| ");
	AST_print(head->lhs);
	strcpy(indent_current, "` ");
	AST_print(head->mid);
	AST_print(head->rhs);
	indent -= 2;
	(*indent_current) = '\0';
}
