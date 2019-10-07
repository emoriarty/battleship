# Battleship (Elm)

## Random Positioning

All ships must be placed randomly in a predefined layout. To prevent 
ships overflowing the grid, ships positioning algorithm must take into account 
the invisible boundaries given by ship size and orientation. Thus, after the 
first ship has been placed, the next in line should consider how much space is 
available according to its length and current availability. Also must be 
considered the space already taken by present ships.

By now, ship orientation randomize takes place before the ships positioning 
began. Perhaps an improved algorithm should choose the ship orientation based 
on current space availability left while doing the positioning.

Another improvement can be not allowing ships with the same orientation be in 
the same line. There is no problem to have ships colliding with different 
orientations, this strategy ussualy helps to confuse the opponent. But ships with 
the same orientation colliding in the same line is not actually a good strategy, 
it is just so easy following that sinking line.

## Custom Positioning

## TODO
* [ ] __[Random Positioning]__: prevent ships with the same orientation being placed in the 
same line when other ship is present.
* [ ] __[Custom Positioning]__: Dragging ships.
* [ ] __[Custom Positioning]__: Change ship orientation.
