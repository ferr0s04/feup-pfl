% dish(Name, Price, IngredientGrams).
dish(pizza,         2200, [cheese-300, tomato-350]).
dish(ratatouille,   2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread,  1600, [cheese-50, garlic-200]).

:- dynamic ingredient/2.

% ingredient(Name, CostPerGram).
ingredient(cheese,   4).
ingredient(tomato,   2).
ingredient(eggplant, 7).
ingredient(garlic,   6).

% Exercício 1: count the number of ingredients in a dish
count_ingredients(Dish, NumIngredients) :-
	dish(Name, Price, IngredientGrams),
	length(IngredientGrams, NumIngredients).

% Exercício 2: calculate the cost of an ingredient
ingredient_amount_cost(Ingredient, Grams, TotalCost) :-
	ingredient(Ingredient, CostPerGram),
	TotalCost is Grams * CostPerGram.

% Exercício 3: calculate the profit of a dish
get_ingredients_cost([], TotalCost, TotalCost).
get_ingredients_cost([Ingredient-Grams|IngredientGrams], Aux, TotalCost) :-
	ingredient_amount_cost(Ingredient, Grams, Price),
	NewAux is Aux + Price,
	get_ingredients_cost(Ingredients, NewAux, TotalCost).

dish_profit(Dish, Profit) :-
    dish(Dish, Price, IngredientGrams),
    get_ingredients_cost(IngredientGrams, 0, TotalCost),
    Profit is Price - TotalCost.

% Exercício 4: update the cost of an ingredient
update_unit_cost(Ingredient, NewUnitCost) :-
    retractall(ingredient(Ingredient, _)),
    assert(ingredient(Ingredient, NewUnitCost)).

% Exercício 5: get the most expensive dish
dish_price(Dish, Price) :- 
    dish(Dish, Price, _).

most_expensive_dish(?Dish, ?Price) :-
    dish_price(Dish, Price),
    \+ (dish_price(_, OtherPrice), OtherPrice > Price).

% Exercício 6: update the stock of an ingredient
consume_ingredient(IngredientStocks, Ingredient, Grams, NewIngredientStocks) :-
	select(Ingredient-Amount, IngredientStocks, RemainingStocks),
	Amount >= Grams,
	NewAmount is Amount - Grams,
	NewIngredientStocks = [Ingredient-NewAmount | RemainingStocks].

% Exercício 7: number of dishes that contain an ingredient
count_dishes_with_ingredient(Ingredient, N) :-
    count_dishes_with_ingredient(Ingredient, 0, N).

count_dishes_with_ingredient(_, N, N) :-
    N > 0.

count_dishes_with_ingredient(Ingredient, Acc, N) :-
    dish(_, _, Ingredients),
    member(Ingredient-_, Ingredients),
    NewAcc is Acc + 1,
    count_dishes_with_ingredient(Ingredient, NewAcc, N).

% Exercício 8: get pairs dish-list of ingredients
list_dishes(DishIngredients) :-
    findall(Dish-Ingredients, dish(Dish, _, Ingredients), DishIngredients).

% Exercício 9: get the list of dishes by profit
most_lucrative_dishes(Dishes):-
	setof(Profit-Dish, dish_profit(Dish, Profit), Costs),
	reverse(Costs, CostsInv),
	findall(Dish, member(_-Dish, CostsInv), Dishes).

%G1
edge(g1, br, o).
edge(g1, br, ni).
edge(g1, o, ni).
edge(g1, o, c).
edge(g1, o, h).
edge(g1, h, c).
edge(g1, h, n).
edge(g1, n, he).
edge(g1, c, he).

% G2
edge(g2, br, h).
edge(g2, br, ni).
edge(g2, h, ni).
edge(g2, h, o).
edge(g2, h, c).
edge(g2, o, c).
edge(g2, o, n).
edge(g2, n, he).
edge(g2, c, he).
edge(g2, cl, he).

% Exercício 21: get common edges (consider both directions)
common_edges(G1, G2, L) :- 
	findall(Orig-Dest, (edge(G1, Orig, Dest), edge(G2, Orig, Dest)), L1),
	findall(Orig-Dest, (edge(G1, Orig, Dest), edge(G2, Dest, Orig)), L2),
	append(L1, L2, L).
