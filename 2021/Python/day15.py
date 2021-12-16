import queue

import numpy as np


def load() -> np.ndarray:
    with open('../.input/day15') as f:
        lines = list(map(str.strip, f.readlines()))
    return np.array([int(num) for line in lines for num in line]).reshape((-1, len(lines)))


def manhattan(node, goal) -> int:
    return abs(node[0] - goal[0]) + abs(node[1] - goal[1])


def neighbors(node, shape) -> list[tuple[int, int]]:
    return list(filter(
        lambda t: 0 <= t[0] < shape[0] and 0 <= t[1] < shape[1],
        [(node[0] + 1, node[1]), (node[0] - 1, node[1]), (node[0], node[1] + 1), (node[0], node[1] - 1)]
    ))


def a_star(
        start: tuple[int, int],
        goal: tuple[int, int],
        grid: np.ndarray,
) -> list[tuple[int, int]]:
    frontier: queue.Queue[tuple[int, tuple[int, int]]] = queue.PriorityQueue()
    parent: dict[tuple[int, int], tuple[int, int]] = {}
    cost: dict[tuple[int, int], int] = {start: 0}

    frontier.put((0, start))
    while not frontier.empty():
        current: tuple[int, int] = frontier.get()[1]
        if current == goal:
            break

        for neighbor in neighbors(current, grid.shape):
            new_cost: int = cost[current] + grid[neighbor[1]][neighbor[0]]
            if neighbor not in cost or new_cost < cost[neighbor]:
                cost[neighbor], parent[neighbor] = new_cost, current
                frontier.put((
                    new_cost + manhattan(neighbor, goal),
                    neighbor
                ))

    path: list[tuple[int, int]] = [goal]
    while path[-1] != start:
        path.append(parent[path[-1]])
    return path[::-1]


def solve1() -> int:
    grid = load()
    path = a_star((0, 0), (len(grid[0]) - 1, len(grid) - 1), grid)
    return sum(grid[y][x] for x, y in path[1:])


def solve2() -> int:
    full_grid = np.tile((grid := load()), (5, 5))
    s_x, s_y = grid.shape[0], grid.shape[1]
    for i in range(5):
        for j in range(5):
            full_grid[i*s_y:i*s_y+s_y, j*s_x:j*s_x+s_x] += np.full_like(grid, i+j)
    full_grid = full_grid % 10 + (full_grid >= 10).astype(int)
    path = a_star((0, 0), (full_grid.shape[0] - 1, full_grid.shape[1] - 1), full_grid)
    return sum(full_grid[y][x] for x, y in path[1:])


if __name__ == "__main__":
    print(solve1())  # 748
    print(solve2())  # 3045
