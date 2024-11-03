# 1# Practical Assignment

**Team Members**  
Gonçalo Miguel Dias Ferros - up202207592@up.pt (50%)  
- Functions: cities, areAdjacent, distance, adjacent, pathDistance, rome, shortestPath.
- Auxiliar Functions: countConnections, bfsPaths
- Contributed for the README writing;

Miguel Moita Caseira - up202207678@up.pt (50%)  
- Functions: isStronglyConnected, travelSales.
- Auxiliar Functions: dfs, distanceMatrix, minimumByLength, tsp.
- Contributed for the README writing;  

Everyone contributed to the main code, sometimes correcting some flaws, but this were the main tasks of each one.

## Description
The goal of this assignment is to develop a Haskell program to set appropriate data types to model a country roadmap as a graph of interconnected cities (nodes) by roads (edges), and to define functions to properly manipulate it and to calculate some metrics such as the shortest path between two cities, and solve the Traveling Salesman Problem (TSP) for the given roadmap.

## shortestPath
The shortestPath function finds all shortest paths between two cities in a roadmap using a breadth-first search (BFS) algorithm.

List of Paths: Maintains paths as lists of cities, allowing for easy construction and exploration.

BFS: This algorithm explores the graph layer by layer, ensuring that when a city is reached, it's via the shortest path. We used it because is effective for unweighted graphs, guaranteeing the shortest path. Using lists for paths enables straightforward concatenation, ensuring all potential paths are evaluated efficiently.

## travelSales
 
 The travelSales function solves the Traveling Salesman Problem (TSP) using dynamic programming.

Adjacency Matrix (AdjMatrix): Stores distances between cities for quick lookups, essential for recursive calculations.

Dynamic Programming with Bit Masking: The function uses recursion and a bitmask to track visited cities, exploring possible paths while accumulating distances. This approach efficiently manages the combinatorial nature of TSP, allowing the function to find the shortest path that visits all cities exactly once, returning to the origin city.