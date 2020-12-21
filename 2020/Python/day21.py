from typing import Tuple


def parse() -> Tuple[set[str], set[str], list[Tuple[list[str], list[str]]]]:
    ingredient_set = set()
    allergen_set = set()
    output = []
    with open("input.txt") as file:
        for line in file:
            ingredients, allergens = line.rstrip().split(" (contains ")
            ingredients_list = list(map(str.strip, ingredients.split(" ")))
            allergens_list = list(map(str.strip, allergens[:-1].split(",")))
            [ingredient_set.add(ingredient) for ingredient in ingredients_list]
            [allergen_set.add(allergen) for allergen in allergens_list]
            output.append((ingredients_list, allergens_list))
    return ingredient_set, allergen_set, output


def predict_ingredients(data: list[Tuple[list[str], list[str]]], allergen: str) -> list[str]:
    counted_occurrences = {}
    for ingredients, allergens in data:
        if allergen in allergens:
            for ingredient in ingredients:
                if ingredient not in counted_occurrences:
                    counted_occurrences[ingredient] = 0
                counted_occurrences[ingredient] += 1

    found_ingredients = set()
    for ingredients, allergens in data:
        if allergen in allergens:
            max_occurrences = max([val if key in ingredients else 0 for (key, val) in counted_occurrences.items()])
            for ingredient in ingredients:
                if counted_occurrences[ingredient] == max_occurrences:
                    found_ingredients.add(ingredient)

    return list(found_ingredients)


def solve_p1(data: list[Tuple[list[str], list[str]]], ingredients: set[str], allergens: set[str]) -> int:
    probable_ingredients = set()
    for allergen in allergens:
        for found in predict_ingredients(data, allergen):
            probable_ingredients.add(found)

    safe = ingredients.difference(probable_ingredients)
    counted = 0
    for ingredients, _ in data:
        for ingredient in ingredients:
            if ingredient in safe:
                counted += 1
    return counted


INGREDIENTS, ALLERGENS, DATA = parse()
print(solve_p1(DATA, INGREDIENTS, ALLERGENS))


def solve_p2(data: list[Tuple[list[str], list[str]]], ingredients: set[str], allergens: set[str]) -> int:
    assigned_allergens = []
    for allergen in allergens:
        print(allergen, predict_ingredients(data, allergen))


solve_p2(DATA, INGREDIENTS, ALLERGENS)
