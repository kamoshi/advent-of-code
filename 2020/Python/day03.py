lines = []

with open("input.txt") as file:
    for line in file:
        lines.append(line.rstrip())


def check_tree(data: list[str], x: int, y: int) -> bool:
    width = len(data[0])
    return data[y][x % width] == '#'


def count_trees_by_slope(data: list[str], slope_x: int, slope_y: int) -> int:
    curr_x, curr_y = 0, 0
    total_tree = 0
    while True:
        curr_x += slope_x
        curr_y += slope_y
        if not curr_y < len(data):
            break
        total_tree += check_tree(data, curr_x, curr_y)
    return total_tree


def solve_p1():
    print(count_trees_by_slope(data=lines, slope_x=3, slope_y=1))


def solve_p2():
    checked = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    result = 1
    for (x, y) in checked:
        result *= count_trees_by_slope(data=lines, slope_x=x, slope_y=y)
    print(result)


solve_p1()
solve_p2()
