def parse_data() -> list[int]:
    numbers = []
    with open("input.txt") as file:
        for line in file:
            numbers += map(int, line.split(","))
    return numbers


def solve(data: list[int], which_number: int) -> int:
    turns = {}
    curr_turn = 1
    last_num = 0

    def shift(_number, _turn) -> None:
        if _number in turns:
            (_turn_before, _turn_last) = turns[_number]
            turns[_number] = (_turn_last, _turn)
        else:
            turns[_number] = (-1, _turn)

    for number in data:
        turns[number] = (-1, curr_turn)
        last_num = number
        curr_turn += 1

    _limit = which_number + 1
    for i in range(curr_turn, _limit):
        if last_num in turns:
            (before, last) = turns[last_num]
            if before != -1:
                age = last - before
                last_num = age
                shift(age, i)
                continue
        last_num = 0
        shift(0, i)

    return last_num


def solve_p1(data: list[int]) -> int:
    return solve(data, which_number=2020)


def solve_p2(data: list[int]) -> int:
    return solve(data, which_number=30000000)


DATA = parse_data()
print(solve_p1(DATA))
print(solve_p2(DATA))
