import re


input_pattern = re.compile(r'([a-zA-Z]+)-([a-zA-Z]+)')
small_pattern = re.compile(r'([a-z]+)')


def load():
    with open('../.input/day12', 'r') as f:
        return input_pattern.findall(f.read())


def construct_paths(graph, start, end, path=None, single_reentry=False):
    if path is None:
        path = [start]

    if start == end:
        yield path
    else:
        for node in graph[start]:
            is_small = small_pattern.match(node)
            if not is_small or node not in path:
                yield from construct_paths(graph, node, end, path + [node], single_reentry)
            elif is_small and single_reentry and node not in ["start", "end"]:
                yield from construct_paths(graph, node, end, path + [node])


def solve1():
    graph = {}
    for node1, node2 in load():
        graph.setdefault(node1, set()).add(node2)
        graph.setdefault(node2, set()).add(node1)

    return sum(1 for _ in construct_paths(graph, 'start', 'end'))


def solve2():
    graph = {}
    for node1, node2 in load():
        graph.setdefault(node1, set()).add(node2)
        graph.setdefault(node2, set()).add(node1)

    return sum(1 for _ in construct_paths(graph, 'start', 'end', single_reentry=True))


if __name__ == '__main__':
    print(solve1())  # 4304
    print(solve2())  # 118242
