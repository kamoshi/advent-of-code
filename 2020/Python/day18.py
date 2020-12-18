from enum import Enum
from typing import Union


class TokenType(Enum):
    Number = 0
    Op1 = 1
    Op2 = 2
    Op3 = 3
    LParen = 8
    RParen = 9
    Invalid = 10


class Token:
    def __init__(self, token_type: TokenType, data: Union[float, str, None]):
        self.token_type = token_type
        self.data = data

    def clone(self):
        return Token(self.token_type, self.data)

    def __str__(self):
        return f"({self.token_type}:{self.data})"


def tokenize_string(string: str) -> list[Token]:
    token_list = []
    for char in string:
        if char.isdigit() or char == '.':
            token_list.append(Token(TokenType.Number, char))
        elif char == '-':
            if not token_list or token_list[-1].token_type not in [TokenType.Number, TokenType.RParen]:
                token_list.append(Token(TokenType.Number, char))
            else:
                token_list.append(Token(TokenType.Op1, char))
        elif char in ['*']:  # Changes to this place change operation precedence
            token_list.append(Token(TokenType.Op1, char))
        elif char in ['+']:
            token_list.append(Token(TokenType.Op2, char))
        elif char == '^':
            token_list.append(Token(TokenType.Op3, char))
        elif char == '(':
            token_list.append(Token(TokenType.LParen, None))
        elif char == ')':
            token_list.append(Token(TokenType.RParen, None))
        else:
            token_list.append(Token(TokenType.Invalid, None))
    return token_list


def simplify_token_list(tokens: list[Token]) -> list[Token]:
    simplified_list = []

    if len(tokens) != 0:
        simplified_list.append(tokens[0].clone())
        for i in range(1, len(tokens)):
            if tokens[i].token_type == TokenType.Number and simplified_list[-1].token_type == TokenType.Number:
                simplified_list[-1].data += tokens[i].data
            else:
                simplified_list.append(tokens[i].clone())

    for token in simplified_list:
        if token.token_type == TokenType.Number:
            if token.data in ['.', '-']:
                token.data = None
                token.token_type = TokenType.Invalid
            elif token.data[:1] == '.':
                token.data = "0" + token.data
            elif token.data[-1:] == '.':
                token.data += '0'

            number_of_dots = 0
            for char in str(token.data):
                if char == '.':
                    number_of_dots += 1
            if number_of_dots > 1:
                token.data = None
                token.token_type = TokenType.Invalid

    return simplified_list


def generate_rpn(token_list: list[Token]) -> list[Token]:
    stack = []
    output = []

    for token in token_list:

        if token.token_type in [TokenType.Number, TokenType.Invalid]:
            output.append(token)

        elif token.token_type == TokenType.LParen:
            stack.append(token)

        elif token.token_type == TokenType.RParen:
            if not stack:
                return [Token(TokenType.Invalid, None)]
            operator = stack.pop()
            while not operator.token_type == TokenType.LParen:
                output.append(operator)
                if not stack:
                    return [Token(TokenType.Invalid, None)]
                operator = stack.pop()

        else:  # Correct but linter says it's not
            while stack and 1 <= stack[-1].token_type.value <= 3 and token.token_type.value <= stack[-1].token_type.value:
                output.append(stack.pop())
            stack.append(token)

    while stack:
        output.append(stack.pop())

    return output


def solve_rpn(rpn_tokens: list[Token]) -> (bool, Union[int, float]):
    stack: list[Union[int, float]] = []
    for token in rpn_tokens:
        if token.token_type == TokenType.Number:
            stack.append(float(token.data) if '.' in token.data else int(token.data))
        else:
            if len(stack) < 2:
                return False, 0
            b = stack.pop()
            a = stack.pop()
            if token.data == '+':
                stack.append(a+b)
            elif token.data == '-':
                stack.append(a-b)
            elif token.data == '*':
                stack.append(a*b)
            elif token.data == '/':
                if b == 0:
                    return False, 0
                stack.append(a/b)
            elif token.data == '^':
                stack.append(a**b)
            else:
                return False, 0
    if len(stack) == 1:
        return True, stack[0]
    else:
        return False, 0


def solve(string: str) -> (bool, float):
    tokenized = tokenize_string(string.replace(" ", ""))
    simplified = simplify_token_list(tokenized)
    rpned = generate_rpn(simplified)
    return solve_rpn(rpned)


def parse_data() -> list[str]:
    data = []
    with open("input.txt") as file:
        for line in file:
            data.append(line.rstrip())
    return data


def solve_p1_p2(data: list[str]) -> int:
    sum_res = 0
    for calc in data:
        succ, res = solve(calc)
        sum_res += res
    return sum_res


DATA = parse_data()
print(solve_p1_p2(DATA))
