from enum import Enum
from typing import Union, Tuple


class Side(Enum):
    UP = 1
    DOWN = 2
    LEFT = 4
    RIGHT = 8


class Tile:
    def __init__(self, uid: str, image: list[list[str]]):
        self.uid = uid
        self.image = image
        self.up: Union[Tile, None] = None
        self.down: Union[Tile, None] = None
        self.left: Union[Tile, None] = None
        self.right: Union[Tile, None] = None

    def flip_ud(self) -> 'Tile':
        cache = self.up
        self.up = self.down
        self.up = cache
        self.image = list(reversed(self.image))
        return self

    def flip_lr(self) -> 'Tile':
        cache = self.left
        self.left = self.right
        self.right = cache
        for i in range(len(self.image)):
            self.image[i] = list(reversed(self.image[i]))
        return self

    def rotate_l(self) -> 'Tile':
        new_image = []
        for i in range(len(self.image[0])):
            new_image.append([])
        for line in self.image:
            for i in range(len(line)):
                new_image[i].append(list(reversed(line))[i])
        self.image = new_image
        return self

    def rotate_r(self) -> 'Tile':
        new_image = []
        for i in range(len(self.image[0])):
            new_image.append([])
        for line in reversed(self.image):
            for i in range(len(line)):
                new_image[i].append(line[i])
        self.image = new_image
        return self

    def align_to(self, edge: str, side: Side):
        if edge not in self.get_all_edges():
            return
        rev = "".join(reversed(edge))
        if side == Side.UP:
            while "".join(self.image[0]) not in [edge, rev]:
                self.rotate_r()
            if "".join(self.image[0]) == rev:
                self.flip_lr()
        elif side == side.DOWN:
            while "".join(self.image[len(self.image)-1]) not in [edge, rev]:
                self.rotate_r()
            if "".join(self.image[len(self.image)-1]) == rev:
                self.flip_lr()
        elif side == side.LEFT:
            while "".join([line[0] for line in self.image]) not in [edge, rev]:
                self.rotate_r()
            if "".join([line[0] for line in self.image]) == rev:
                self.flip_ud()
        elif side == side.RIGHT:
            while "".join([line[len(self.image[0])-1] for line in self.image]) not in [edge, rev]:
                self.rotate_r()
            if "".join([line[len(self.image[0])-1] for line in self.image]) == rev:
                self.flip_ud()

    def get_all_edges(self) -> list[str]:
        all_edges = []
        up = "".join(self.image[0])
        down = "".join(self.image[len(self.image)-1])
        left = "".join([line[0] for line in self.image])
        right = "".join([line[len(self.image[0])-1] for line in self.image])
        all_edges += [up, down, left, right, "".join(reversed(up)), "".join(reversed(down)), "".join(reversed(left)), "".join(reversed(right))]
        return all_edges

    def count_connections(self) -> int:
        return sum([1 if x else 0 for x in [self.up, self.down, self.left, self.right]])

    def print(self):
        print(f"Tile {self.uid}:")
        for line in self.image:
            print("".join(line))

    def __str__(self):
        return f"Tile {self.uid}"


def parse_data() -> list[Tile]:
    output = []
    current_img = [['.']]
    current_title = ""
    with open("input.txt") as file:
        for line in file:
            line = line.rstrip()
            if not line:
                continue

            if line[0] == 'T':
                output.append(Tile(current_title, current_img))
                current_title = line.split(" ")[1][:-1]
                current_img = []
            elif line[0] in ['#', '.']:
                current_img.append(list(line))
        output.append(Tile(current_title, current_img))
    return output[1:]


def connect_tiles(data: list[Tile], already_done: set[str], tile: Tile):
    if tile.uid in already_done:
        return
    already_done.add(tile.uid)

    up = "".join(tile.image[0])
    down = "".join(tile.image[len(tile.image) - 1])
    left = "".join([line[0] for line in tile.image])
    right = "".join([line[len(tile.image[0]) - 1] for line in tile.image])
    for other_tile in data:
        if other_tile.uid in already_done or tile.uid == other_tile.uid:
            continue
        other_edges = other_tile.get_all_edges()
        if up in other_edges:
            other_tile.align_to(up, Side.DOWN)
            tile.up = other_tile
            other_tile.down = tile
        elif down in other_edges:
            other_tile.align_to(down, Side.UP)
            tile.down = other_tile
            other_tile.up = tile
        elif left in other_edges:
            other_tile.align_to(left, Side.RIGHT)
            tile.left = other_tile
            other_tile.right = tile
        elif right in other_edges:
            other_tile.align_to(right, Side.LEFT)
            tile.right = other_tile
            other_tile.left = tile
    if tile.up:
        connect_tiles(data, already_done, tile.up)
    if tile.down:
        connect_tiles(data, already_done, tile.down)
    if tile.left:
        connect_tiles(data, already_done, tile.left)
    if tile.right:
        connect_tiles(data, already_done, tile.right)


def solve_p1(data: list[Tile]) -> int:
    result = 1
    for tile in data:
        if tile.count_connections() == 2:
            result *= int(tile.uid)
    return result


# SOLVED PART 1
DATA = parse_data()
connect_tiles(DATA, set(), DATA[0].flip_ud())
print(solve_p1(DATA))


def render_full_image(data: list[Tile]) -> Tile:
    # Find top-left corner
    current_start: Union[None, Tile] = None
    for tile in data:
        if not tile.left and not tile.up and tile.right and tile.down:
            current_start = tile
            break
    rendered = [[]]
    while current_start:
        for i in range(1, len(current_start.image[0])-1):
            line = []
            line += current_start.image[i][1:-1]

            current_right = current_start.right
            while current_right:  # add line from the right side
                line += current_right.image[i][1:-1]
                current_right = current_right.right

            rendered.append(line)
        current_start = current_start.down

    return Tile("RENDERED", rendered[1:])


def scan_window(window: list[list[str]], start_r: int, start_c: int) -> list[Tuple[int, int]]:
    if len(window) != 3 or len(window[0]) != 20:
        return []
    required = [(0, 18), (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1, 18), (1, 19), (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)]
    for r, c in required:
        if window[r][c] != "#":
            return []
    return [(r+start_r, c+start_c) for (r, c) in required]


def scan_image(image: list[list[str]]) -> set[Tuple[int, int]]:
    found = set()
    for i in range(len(image)-2):
        for j in range(len(image[0])-19):
            window = [image[i][j:j+20], image[i+1][j:j+20], image[i+2][j:j+20]]
            for found_r, found_c in scan_window(window, start_r=i, start_c=j):
                found.add((found_r, found_c))
    return found


def solve_p2(data: list[Tile]) -> int:
    rendered = render_full_image(data)
    scan_set = set()
    for i in range(4):
        scan_set = scan_image(rendered.image)
        if len(scan_set) > 0:
            break
        rendered.rotate_r()
    if len(scan_set) == 0:
        rendered.flip_lr()
        for i in range(4):
            scan_set = scan_image(rendered.image)
            if len(scan_set) > 0:
                break
            rendered.rotate_r()

    # Find not marked characters
    counted = 0
    for r in range(len(rendered.image)):
        for c in range(len(rendered.image[0])):
            if rendered.image[r][c] == '#' and (r, c) not in scan_set:
                counted += 1
    return counted


# SOLVED PART 2
print(solve_p2(DATA))
