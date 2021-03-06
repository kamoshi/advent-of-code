
def parse_data() -> list[list[bool]]:
    input_data = []
    with open("input.txt") as file:
        for line in file:
            input_data.append([True if x == '#' else False for x in line.rstrip()])

    return input_data


class Space:
    def __init__(self, row: int, col: int, height: int):
        self.space: list[list[list[bool]]] = []
        self.row = row
        self.col = col
        self.height = height
        self._offset_row = 0
        self._offset_col = 0
        self._offset_h = 0

        for h in range(self.height):
            level_l = []
            for row in range(self.row):
                row_l = []
                for col in range(self.col):
                    row_l.append(False)
                level_l.append(row_l)
            self.space.append(level_l)

    # Loads data with a given offset
    def load_data(self, data: list[list[bool]], offset_row: int, offset_col: int, offset_h: int) -> None:
        self._offset_row = offset_row
        self._offset_col = offset_col
        self._offset_h = offset_h

        for row in range(self._offset_row, len(data) + self._offset_row):
            for col in range(self._offset_col, len(data[0]) + self._offset_col):
                self.space[self._offset_h][row][col] = data[row-self._offset_row][col-self._offset_col]

    # Get activation value from absolute x,y,z, False if outside bounds
    def get(self, row: int, col: int, h: int) -> bool:
        if 0 <= h < self.height and 0 <= row < self.row and 0 <= col < self.col:
            return self.space[h][row][col]
        return False

    # Set activation value for absolute x,y,z, crash if outside bounds
    def set(self, row: int, col: int, h: int, value: bool) -> None:
        self.space[h][row][col] = value

    # Get activation value from offset x,y,z, False if outside bounds
    def get_o(self, row: int, col: int, h: int):
        return self.get(row+self._offset_row, col+self._offset_col, h+self._offset_h)

    # Set activation value for offset x,y,z, crash if outside bounds
    def set_o(self, row: int, col: int, h: int, value: bool) -> None:
        self.set(row+self._offset_row, col+self._offset_col, h+self._offset_h, value)

    # Counts activated neighbours for a given absolute position
    def count_neighbours(self, row: int, col: int, h: int) -> int:
        checked = [(-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),(0,-1,-1),(0,-1,0),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,0),(0,1,1),(1,-1,-1),(1,-1,0),(1,-1,1),(1,0,-1),(1,0,0),(1,0,1),(1,1,-1),(1,1,0),(1,1,1)]

        count = 0
        for col_o, row_o, h_o in checked:
            if self.get(row=row+row_o, col=col+col_o, h=h+h_o):
                count += 1

        return count

    # Counts activated neighbours for a given offset position
    def count_neighbours_o(self, row: int, col: int, h: int) -> int:
        return self.count_neighbours(row+self._offset_row, col+self._offset_col, h+self._offset_h)

    # Get the next state of the space
    def next(self) -> 'Space':
        next_space: Space = Space(self.col, self.row, self.row)
        for h in range(self.height):
            for r in range(self.row):
                for c in range(self.col):
                    active_ns = self.count_neighbours(r, c, h)
                    active = self.get(r, c, h)
                    if active:
                        next_space.set(r, c, h, active_ns in [2, 3])
                    else:
                        next_space.set(r, c, h, active_ns == 3)

        return next_space

    # Count all activated values
    def count_activated(self) -> int:
        counted = 0

        for level in self.space:
            for column in level:
                counted += sum(column)

        return counted

    # Print at level
    def print_level(self, level: int):
        level_grid = self.space[level]
        for row in level_grid:
            print(''.join(['#' if x else '.' for x in row]))


class Space4D:
    def __init__(self, row: int, col: int, height: int, w_dim: int):
        self.space: list[list[list[list[bool]]]] = []
        self.row = row
        self.col = col
        self.height = height
        self.w_dim = w_dim
        self._offset_row = 0
        self._offset_col = 0
        self._offset_h = 0
        self._offset_w = 0

        for h in range(self.height):
            level_l = []
            for row in range(self.row):
                row_l = []
                for col in range(self.col):
                    w_dim_l = []
                    for w in range(self.w_dim):
                        w_dim_l.append(False)
                    row_l.append(w_dim_l)
                level_l.append(row_l)
            self.space.append(level_l)

    # Loads data with a given offset
    def load_data(self, data: list[list[bool]], offset_row: int, offset_col: int, offset_h: int, offset_w: int) -> None:
        self._offset_row = offset_row
        self._offset_col = offset_col
        self._offset_h = offset_h
        self._offset_w = offset_w

        for row in range(self._offset_row, len(data) + self._offset_row):
            for col in range(self._offset_col, len(data[0]) + self._offset_col):
                self.space[self._offset_h][row][col][self._offset_w] = data[row-self._offset_row][col-self._offset_col]

    # Get activation value from absolute x,y,z, False if outside bounds
    def get(self, row: int, col: int, h: int, w_dim: int) -> bool:
        if 0 <= h < self.height and 0 <= row < self.row and 0 <= col < self.col and 0 <= w_dim < self.w_dim:
            return self.space[h][row][col][w_dim]
        return False

    # Set activation value for absolute x,y,z, crash if outside bounds
    def set(self, row: int, col: int, h: int, w_dim: int, value: bool) -> None:
        self.space[h][row][col][w_dim] = value

    # Get activation value from offset x,y,z, False if outside bounds
    def get_o(self, row: int, col: int, h: int, w_dim: int):
        return self.get(row+self._offset_row, col+self._offset_col, h+self._offset_h, w_dim+self._offset_w)

    # Set activation value for offset x,y,z, crash if outside bounds
    def set_o(self, row: int, col: int, h: int, w_dim: int, value: bool) -> None:
        self.set(row+self._offset_row, col+self._offset_col, h+self._offset_h, w_dim+self._offset_w, value)

    # Counts activated neighbours for a given absolute position
    def count_neighbours(self, row: int, col: int, h: int, w_dim: int) -> int:
        checked = [(-1,-1,-1,-1),(-1,-1,-1,0),(-1,-1,-1,1),(-1,-1,0,-1),(-1,-1,0,0),(-1,-1,0,1),(-1,-1,1,-1),
                   (-1,-1,1,0),(-1,-1,1,1),(-1,0,-1,-1),(-1,0,-1,0),(-1,0,-1,1),(-1,0,0,-1),(-1,0,0,0),
                   (-1,0,0,1),(-1,0,1,-1),(-1,0,1,0),(-1,0,1,1),(-1,1,-1,-1),(-1,1,-1,0),(-1,1,-1,1),(-1,1,0,-1),
                   (-1,1,0,0),(-1,1,0,1),(-1,1,1,-1),(-1,1,1,0),(-1,1,1,1),(0,-1,-1,-1),(0,-1,-1,0),(0,-1,-1,1),
                   (0,-1,0,-1),(0,-1,0,0),(0,-1,0,1),(0,-1,1,-1),(0,-1,1,0),(0,-1,1,1),(0,0,-1,-1),(0,0,-1,0),
                   (0,0,-1,1),(0,0,0,-1),(0,0,0,1),(0,0,1,-1),(0,0,1,0),(0,0,1,1),(0,1,-1,-1),(0,1,-1,0),
                   (0,1,-1,1),(0,1,0,-1),(0,1,0,0),(0,1,0,1),(0,1,1,-1),(0,1,1,0),(0,1,1,1),(1,-1,-1,-1),
                   (1,-1,-1,0),(1,-1,-1,1),(1,-1,0,-1),(1,-1,0,0),(1,-1,0,1),(1,-1,1,-1),(1,-1,1,0),(1,-1,1,1),
                   (1,0,-1,-1),(1,0,-1,0),(1,0,-1,1),(1,0,0,-1),(1,0,0,0),(1,0,0,1),(1,0,1,-1),(1,0,1,0),(1,0,1,1),
                   (1,1,-1,-1),(1,1,-1,0),(1,1,-1,1),(1,1,0,-1),(1,1,0,0),(1,1,0,1),(1,1,1,-1),(1,1,1,0),(1,1,1,1)]

        count = 0
        for col_o, row_o, h_o, w_o in checked:
            if self.get(row=row+row_o, col=col+col_o, h=h+h_o, w_dim=w_dim+w_o):
                count += 1
        return count

    # Counts activated neighbours for a given offset position
    def count_neighbours_o(self, row: int, col: int, h: int, w_dim: int) -> int:
        return self.count_neighbours(row+self._offset_row, col+self._offset_col, h+self._offset_h, w_dim+self._offset_w)

    # Get the next state of the space
    def next(self) -> 'Space4D':
        next_space: Space4D = Space4D(self.col, self.row, self.row, self.w_dim)
        for h in range(self.height):
            for r in range(self.row):
                for c in range(self.col):
                    for w in range(self.w_dim):
                        active_ns = self.count_neighbours(r, c, h, w)
                        active = self.get(r, c, h, w)
                        if active:
                            next_space.set(r, c, h, w, active_ns in [2, 3])
                        else:
                            next_space.set(r, c, h, w, active_ns == 3)

        return next_space

    # Count all activated values
    def count_activated(self) -> int:
        counted = 0

        for level in self.space:
            for column in level:
                for w_dim in column:
                    counted += sum(w_dim)

        return counted

    # Print at level
    def print_level(self, level: int, w_level: int):
        level_grid = self.space[level]
        for row in level_grid:
            print(''.join(['#' if x[w_level] else '.' for x in row]))


def solve_p1(data: list[list[bool]]) -> int:

    def generate_space(_data: list[list[bool]], max_turns: int) -> Space:
        size_row = len(data[0]) + (max_turns * 2)
        size_col = len(data) + (max_turns * 2)
        _space: Space = Space(row=size_row, col=size_col, height=(max_turns * 2) + 1)
        _space.load_data(data=data, offset_col=max_turns, offset_row=max_turns, offset_h=max_turns)
        return _space

    turns = 6
    space: Space = generate_space(data, max_turns=turns)

    for i in range(turns):
        space = space.next()

    return space.count_activated()


def solve_p2(data: list[list[bool]]) -> int:

    def generate_space_4d(_data: list[list[bool]], max_turns: int) -> Space4D:
        size_row = len(data[0]) + (max_turns * 2)
        size_col = len(data) + (max_turns * 2)
        _space: Space4D = Space4D(row=size_row, col=size_col, height=(max_turns * 2) + 1, w_dim=(max_turns * 2) + 1)
        _space.load_data(data=data, offset_col=max_turns, offset_row=max_turns, offset_h=max_turns, offset_w=max_turns)
        return _space

    turns = 6
    space: Space4D = generate_space_4d(data, turns)

    for i in range(turns):
        space = space.next()

    return space.count_activated()


DATA = parse_data()
print(solve_p1(DATA))
print(solve_p2(DATA))
