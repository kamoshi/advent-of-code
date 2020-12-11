import operator
from functools import reduce

data = []

with open("input.txt") as file:
    for line in file:
        _line = line.rstrip()
        data.append(int(_line))


def solve_p1(input_data) -> int:
    _sorted_data = sorted(data)
    sorted_data = [0] + _sorted_data + [_sorted_data[-1] + 3]
    diff = {
        1: 0,
        2: 0,
        3: 0
    }
    for i in range(1, len(sorted_data)):
        diff[sorted_data[i] - sorted_data[i - 1]] += 1
    return diff[1] * diff[3]


# Warning
# this is correct algorithmically, but naive, so extremely slow for long lists
# Saving this because it's nevertheless interesting approach IMO
def naive_solve_p2(input_data) -> int:
    _sorted_data = sorted(input_data)
    sorted_data = [0] + _sorted_data + [_sorted_data[-1] + 3]

    def rec_seek_remove(start_index, partial_data):
        result = 1  # we start with 1 because we have the option to do nothing

        for i in range(start_index, len(partial_data) - 1):
            diff = partial_data[i + 1] - partial_data[i - 1]
            if diff <= 3:  # can be deleted
                new_list = partial_data[:i] + partial_data[i + 1:]
                result += rec_seek_remove(i, new_list)

        return result

    return rec_seek_remove(1, sorted_data)


# This is much faster and still retains a similar approach to the naive algorithm
def solve_p2(input_data) -> int:
    _sorted_data = sorted(input_data)
    sorted_data = [0] + _sorted_data + [_sorted_data[-1] + 3]

    def find_static_indices(_input_data: list[int]) -> list[int]:
        static = []
        for i in range(1, len(_input_data)-1):
            diff = _input_data[i + 1] - _input_data[i - 1]
            if diff > 3:
                static.append(i)
        return [0] + static + [len(_input_data)-1]

    def find_removable_segments(static_indices: list[int]) -> list[range]:
        _segments = []
        for i in range(1, len(static_indices)-1):
            if static_indices[i - 1] + 1 != static_indices[i]:
                _segments.append(range(static_indices[i - 1], static_indices[i]))
        return _segments

    def rec_seek_remove(start_index, partial_data):
        result = 1  # we start with 1 because we have the option to do nothing

        for i in range(start_index, len(partial_data) - 1):
            diff = partial_data[i + 1] - partial_data[i - 1]
            if diff <= 3:  # can be deleted
                new_list = partial_data[:i] + partial_data[i + 1:]
                result += rec_seek_remove(i, new_list)

        return result

    static = find_static_indices(sorted_data)
    segments = find_removable_segments(static)
    results = []
    for segment in segments:
        results.append(rec_seek_remove(1, sorted_data[segment.start:segment.stop+1]))
    return reduce(operator.mul, results)


print(solve_p1(data))
print(solve_p2(data))
