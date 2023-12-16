import random
import time
import sys
import os
from grid import grid


# Option 1 function: Guessing a pair of cells
def guess_pair(grid):
    size = grid.size

    # first cell coordinates
    while True:
        cell1 = input("Enter the coordinates of the first cell (e.g., A0): ")
        if validate_cell(cell1, size):
            break

    # second cell coordinates
    while True:
        cell2 = input("Enter the coordinates of the second cell (e.g., B1): ")
        if validate_cell(cell2, size):
            if(cell1 != cell2):
                break
            else:
                print("Cell cannot be same. Input cell again.")

    # Using the coordinates of a cell, extract the row and column values.
    row1, col1 = convert_cell(cell1)
    row2, col2 = convert_cell(cell2)

    # Show the numbers in the cells you've chosen.
    number1, visible1 = grid.grid[row1][col1]
    number2, visible2 = grid.grid[row2][col2]
    grid.grid[row1][col1] = (number1, True)
    grid.grid[row2][col2] = (number2, True)

    # Show the revised grid
    grid.display_grid(grid)

    # Compare the figures in the cells you've chosen.
    if number1 == number2:
        print("You found a pair!")
        return True
    else:
        print("Not a match.")

        # after two seconds, hide the digits
        time.sleep(2)
        os.system('cls' if os.name == 'nt' else 'clear')
        grid.grid[row1][col1] = (number1, False)
        grid.grid[row2][col2] = (number2, False)
        grid.display_grid(grid)
        return False


# Option 2's function: Manually reveal a cell.
def reveal_cell(grid):
    size = grid.size

    # Get the cell coordinates to reveal
    while True:
        cell = input("Enter the coordinates of the cell to reveal (e.g., C2): ")
        if validate_cell(cell, size):
            break

    # Using the coordinates of a cell, extract the row and column values.
    row, col = convert_cell(cell)

    # Show the numbers in the cells you've chosen.
    number, visible = grid.grid[row][col]
    grid.grid[row][col] = (number, True)

    # Show the revised grid
    grid.display_grid(grid)


#Function to reveal the whole grid
def reveal_game(grid):
    size=grid.size

    # Show column labels
    print(" ", end=" ")
    for col in range(size):
        print(chr(col + ord('A')), end=" ")
    print()

    # Rows with grid content are shown.
    for row in range(size):
        print(row, end=" ")
        for col in range(size):
            number, visible = grid.grid[row][col]
            print(number, end=" ")
        print()

def newGame(grid):
    for row in range (grid.size) :
        for col in range (grid.size) :
            grid.grid[row][col]=(grid.grid[row][col],False)

# Validate cell coordinates
def validate_cell(cell, size):
    if len(cell) != 2:
        return False
    col = cell[0].upper()
    row = cell[1]

    #Check for valid column
    if(col.isalpha() and col.isupper() and col>='A'):
        if(col >= chr(ord('A')+size)):
            print("Input error. Column entry is out of range for this grid. Please try again.")
    
    #Check for valid row
    if(row.isdigit() and row.isupper and int(row) > 0):
        if(int(row)>= size):
            print("Input error. Row entry is out of range for this grid. Please try again.")

    return col.isalpha() and col.isupper() and col >= 'A' and col < chr(ord('A') + size) and row.isdigit() and int(row) >= 0 and int(row) < size

# cell coordinates to row and column values function
def convert_cell(cell):
    col = cell[0].upper()
    row = int(cell[1])
    return row, ord(col) - ord('A')

# calculate the score
def calculate_score(min_guesses, actual_guesses):
    if actual_guesses == 0:
        return 0
    else:
        return (min_guesses / actual_guesses) * 100

# Main
def play_game(grid):
    #grid = grid.initialize_grid(size)
    grid.display_grid(grid)
    size=grid.size
    min_guesses = size * size // 2
    actual_guesses = 0
    valid_guess=False

    while True:
        print("Menu Options:")
        print("1. Guess a pair of cells")
        print("2. Manually Reveal a cell")
        print("3. Give up - Reveal the whole grid")
        print("4. Start a new game")
        print("5. Quit")
        option = input("Enter your choice (1-5): ")

        if option == "1":
            success = guess_pair(grid)
            actual_guesses += 1
            if success:
                min_guesses -= 1
                valid_guess=True
        elif option == "2":
            reveal_cell(grid)
            actual_guesses += 2
        elif option == "3":
            reveal_game(grid)
            print("You gave up!")
            break
        elif option == "4":
            newGame(grid)
            play_game(grid)
            break
        elif option == "5":
            print("Thanks for playing!")
            break
        else:
            print("Invalid option. Try again.")

        # Check if all pairs have been found
        complete=True
        size=grid.size
        for row in range(size):
            for col in range(size):
                number,visible=grid.grid[row][col]
                if(not visible):
                    complete=False
                    break
        if complete:
            if valid_guess:
                score = calculate_score(size * size // 2, actual_guesses)
                print("Congratulations! You found all pairs!")
                print(f"Your score is: {score:.1f}")
                break
            else:
                print("You cheated - LOSER! You're score is 0!")
                break

# Start the game
if __name__ == "__main__":

    if(len(sys.argv) > 1):
        size=int(sys.argv[1])
        if size in [2, 4, 6]:
            grid=grid(size)
            play_game(grid) 
        else:
            print("Invalid grid size. The size must be 2, 4, or 6.")
    else:
        print("Please pass the grid size.")
