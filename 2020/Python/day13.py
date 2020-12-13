def get_data() -> (int, list[int]):
    with open("input.txt") as file:
        time = int(file.readline().rstrip())
        buses = file.readline().rstrip().split(',')
    return time, buses


def solve_p1(time: int, buses: list[str]) -> int:

    def find_earliest_departure(_bus_id: int, _time: int) -> int:
        _departure = 0
        while _departure < _time:
            _departure += _bus_id
        return _departure

    times = []
    for bus in buses:
        if bus == 'x':
            continue
        times.append((int(bus), find_earliest_departure(_bus_id=int(bus), _time=time)))
    sort = sorted(times, key=lambda x: x[1])
    return sort[0][0] * (sort[0][1] - time)


def solve_p2(buses):
    pass


TIME, BUSES = get_data()
print(solve_p1(TIME, BUSES))
print(solve_p2(BUSES))
