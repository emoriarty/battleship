# Battleship (Elm)

## Random positioning

All ships must be placed randomly in a predefined layout. To prevent 
ships overflowing the grid, ships positioning algorithm must take into account 
the invisible boundaries given by ship size and orientation. Thus, after the 
first ship has been placed, the next in line should consider how much space is 
available according to its length, and orientation should be choosen based on 
current availability. Also must be considered the space already took by present 
ships.
