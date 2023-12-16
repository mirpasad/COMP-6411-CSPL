import random

class grid:
    
# A method to initialise the grid using random integer pairs
    def __init__(self, size) -> None:
        self.size=size
        self.numbers = list(range(size*size//2)) * 2
        random.shuffle(self.numbers)
        self.grid = [[(0, True) for _ in range(size)] for _ in range(size)]
        for row in range(size):
            for col in range(size):
                self.grid[row][col] = (self.numbers[row*size+col], False)
        

# Feature that displays the grid
    def display_grid(self, grid):
        print("-----------------")
        print("|   PEEK-A-BOO   |")
        print("-----------------")
        size = self.size
        # Display column labels
        print(" ", end=" ")
        for col in range(size):
            print(chr(col + ord('A')), end=" ")
        print()
        # Display rows with grid contents
        for row in range(size):
            print(row, end=" ")
            for col in range(size):
                number, visible = self.grid[row][col]
                if visible:
                    print(number, end=" ")
                else:
                    print("X", end=" ")
            print()