from typing import Union


def parse_data() -> list[str]:
    data = []
    with open("input.txt") as file:
        for line in file:
            _line = line.rstrip()
            data.append(_line)
    return data


def check_occupied(x: int, y: int, data: list[str]) -> int:
    if 0 <= x < len(data[0]) and 0 <= y < len(data):
        if data[y][x] == '#':
            return 1
    return 0


def count_occu_adj(x: int, y: int, data: list[str]) -> int:
    result = 0
    for _x in range(x - 1, x + 2):
        for _y in range(y - 1, y + 2):
            if _x == 0 and _y == 0:
                continue
            result += check_occupied(_x, _y, data)
    return result


def generate_new_grid(past_data: list[str]):
    output = []
    for y in range(len(past_data)):
        line = ""
        for x in range(len(past_data[0])):
            if past_data[y][x] == "#" and count_occu_adj(x, y, past_data) > 4:
                new_char = 'L'
            elif past_data[y][x] == 'L' and count_occu_adj(x, y, past_data) == 0:
                new_char = '#'
            else:
                new_char = past_data[y][x]
            line += new_char
        output.append(line)
    return output


def is_it_different(old_data: list[str], new_data: list[str]) -> bool:
    for y in range(len(old_data)):
        for x in range(len(old_data[0])):
            if old_data[y][x] != new_data[y][x]:
                return True
    return False


def find_stable_state(grid: list[str], limit_iter: int = 10000) -> Union[None, list[str]]:
    for i in range(limit_iter):
        new_state = generate_new_grid(grid)
        if not is_it_different(grid, new_state):
            return new_state
        grid = new_state
    return None

# Works for the example,
# doesn't work for the real imput???
# Need to do something later
def solve_p1(input_data: list[str]) -> int:
    final_state = find_stable_state(input_data)
    if final_state is None:
        return -1
    result = 0
    print(final_state)
    for y in range(len(final_state)):
        for x in range(len(final_state[0])):
            if final_state[y][x] == '#':
                result += 1
    return result


DATA = parse_data()
print(solve_p1(DATA))
