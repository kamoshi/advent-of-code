from enum import Enum
from typing import Tuple


class Direction(Enum):
    E = 0
    SE = 1
    SW = 2
    W = 3
    NW = 4
    NE = 5


def convert_string(path: str) -> list[Direction]:
    out, i = [], 0
    while i < len(path):
        char = path[i]
        if char in ['s', 'n']:
            i += 1
            char += path[i]
        if char == "e":
            out.append(Direction.E)
        elif char == "w":
            out.append(Direction.W)
        elif char == "ne":
            out.append(Direction.NE)
        elif char == "nw":
            out.append(Direction.NW)
        elif char == "se":
            out.append(Direction.SE)
        elif char == "sw":
            out.append(Direction.SW)
        i += 1
    return out


def determine_neighbour(row: int, col: int, direction: Direction) -> Tuple[int, int]:
    if direction == Direction.E:
        return row, col+1
    elif direction == Direction.SE:
        return (row+1, col+1) if row % 2 == 0 else (row+1, col)
    elif direction == Direction.SW:
        return (row+1, col) if row % 2 == 0 else (row+1, col-1)
    elif direction == Direction.W:
        return row, col-1
    elif direction == Direction.NW:
        return (row-1, col) if row % 2 == 0 else (row-1, col-1)
    elif direction == Direction.NE:
        return (row-1, col+1) if row % 2 == 0 else (row-1, col)


def find_tile(start_r: int, start_c: int, path: str) -> Tuple[int, int]:
    path = convert_string(path)
    tile_r, tile_c = start_r, start_c
    for direction in path:
        tile_r, tile_c = determine_neighbour(tile_r, tile_c, direction)
    return tile_r, tile_c


def parse_data() -> list[str]:
    out = []
    with open("input.txt") as file:
        for line in file:
            out.append(line.rstrip())
    return out


def solve_p1(data: list[str]) -> int:
    black_tiles: set[Tuple[int, int]] = set()
    for line in data:
        found_tile = find_tile(0, 0, line)
        if found_tile in black_tiles:
            black_tiles.remove(found_tile)
        else:
            black_tiles.add(found_tile)
    return len(black_tiles)


DATA = parse_data()
print(solve_p1(DATA))


# Part 2 is kind of done ad hoc today because christmas :p
# not much time to work on it
class Grid:
    def __init__(self, row: int, col: int):
        self.row = row
        self.col = col
        self.grid = []
        for i in range(row):
            row_list = []
            for j in range(col):
                row_list.append(False)
            self.grid.append(row_list)

    def flip(self, row: int, col: int, state: bool) -> None:
        self.grid[row][col] = state

    def get_state(self, row: int, col: int) -> bool:
        if 0 <= row < len(self.grid) and 0 <= col < len(self.grid[0]):
            return self.grid[row][col]
        return False

    def count_ns(self, row: int, col: int) -> int:
        if row % 2 == 0:
            ns = [(0, 1), (0, -1), (1, 1), (1, 0), (-1, 1), (-1, 0)]
        else:
            ns = [(0, 1), (0, -1), (1, 0), (1, -1), (-1, 0), (-1, -1)]
        counted = 0
        for ns_r, ns_c in ns:
            counted += self.get_state(row+ns_r, col+ns_c)
        return counted

    def next(self) -> 'Grid':
        next_grid = Grid(self.row, self.col)
        for i in range(self.row):
            for j in range(self.col):
                active_ns = self.count_ns(i, j)
                active = self.get_state(i, j)
                if active and (active_ns == 0 or active_ns > 2):
                    next_grid.flip(i, j, state=False)
                elif not active and active_ns == 2:
                    next_grid.flip(i, j, state=True)
                else:
                    next_grid.flip(i, j, state=active)
        return next_grid

    def count_black(self):
        counted = 0
        for i in range(self.row):
            for j in range(self.col):
                counted += self.get_state(i, j)
        return counted


grid = Grid(row=200, col=200)
black_tiles: set[Tuple[int, int]] = set()
for line in DATA:
    found_tile = find_tile(0, 0, line)
    if found_tile in black_tiles:
        black_tiles.remove(found_tile)
    else:
        black_tiles.add(found_tile)


for (x, y) in black_tiles:
    grid.flip(x+100, y+100, True)

for i in range(100):
    grid = grid.next()

print(grid.count_black())
