import numpy as np


def loader_np() -> np.ndarray:
    with open('../.input/day04', 'r') as f:
        data = f.read().splitlines()
    
    vector = np.array([int(x) for x in data[0].split(",")])
    tensor = np.array([
        np.array(list(map(np.int32, map(str.split, board))))
        for board
        in [data[i:i+5] for i in range(2, len(data), 6)]
    ])
    return vector, tensor


def solve1() -> int:
    vector, tensor = loader_np()
    for scale in range(1, len(vector)+1):
        for matrix in tensor:
            mask = np.isin(matrix, vector[:scale])
            if mask.all(axis=0).any() or mask.all(axis=1).any():
                return (matrix * ~mask).sum() * vector[:scale][-1]
        


if __name__ == '__main__':
    print(solve1())