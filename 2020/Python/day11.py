from typing import Union, Callable


def parse_data() -> list[str]:
    data = []
    with open("input.txt") as file:
        for line in file:
            _line = line.rstrip()
            data.append(_line)
    return data


# Count occupied seats *adjacent* to the seat at position (x, y)
def count_adjacent(x: int, y: int, data: list[str]) -> int:

    def check_occupied(_x: int, _y: int, _data: list[str]) -> int:
        if 0 <= _x < len(data[0]) and 0 <= _y < len(data):
            if data[_y][_x] == '#':
                return 1
        return 0

    places = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    result = 0
    for _x, _y in places:
        result += check_occupied(x + _x, y + _y, data)
    return result


def generate_new_grid(past_data: list[str], counting_func: Callable, threshold: int):
    output = []
    for y in range(len(past_data)):
        line = ""
        for x in range(len(past_data[0])):
            if past_data[y][x] == "#" and counting_func(x, y, past_data) >= threshold:
                new_char = 'L'
            elif past_data[y][x] == 'L' and counting_func(x, y, past_data) == 0:
                new_char = '#'
            else:
                new_char = past_data[y][x]
            line += new_char
        output.append(line)
    return output


def find_stable_state(grid: list[str], counting_func: Callable, threshold: int = 4, limit_iter: int = 10000) -> Union[None, list[str]]:

    def is_it_different(old_data: list[str], new_data: list[str]) -> bool:
        for y in range(len(old_data)):
            for x in range(len(old_data[0])):
                if old_data[y][x] != new_data[y][x]:
                    return True
        return False

    for i in range(limit_iter):
        new_state = generate_new_grid(grid, counting_func, threshold)
        if not is_it_different(grid, new_state):
            return new_state
        grid = new_state
    return None


def solve_p1(input_data: list[str]) -> int:
    final_state = find_stable_state(input_data, counting_func=count_adjacent, threshold=4)
    if final_state is None:
        return -1
    result = 0
    for line in final_state:
        result += line.count('#')
    return result


# Count occupied seats *raycast* from the seat at position (x, y)
def count_raycast(x: int, y: int, data: list[str]) -> int:

    def check_occupied_ray(start: (int, int), direction: (int, int), _data: list[str]) -> int:
        curr_x, curr_y = start
        dir_x, dir_y = direction
        while 0 <= (curr_x := curr_x + dir_x) < len(data[0]) and 0 <= (curr_y := curr_y + dir_y) < len(data):
            if data[curr_y][curr_x] == '#':
                return 1
            elif data[curr_y][curr_x] == 'L':
                return 0
        return 0

    directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    result = 0
    for direction in directions:
        result += check_occupied_ray((x, y), direction, data)
    return result


def solve_p2(input_data: list[str]) -> int:
    final_state = find_stable_state(input_data, counting_func=count_raycast, threshold=5)
    if final_state is None:
        return -1
    result = 0
    for line in final_state:
        result += line.count('#')
    return result


DATA = parse_data()
print(solve_p1(DATA))
print(solve_p2(DATA))
