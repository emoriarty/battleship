# Battleship (Elm)

## Positioning

The grid where ships are set down is what in mathematics is called a 
Cartesian plane. In the battleship game, there's no need for the whole plane,
only with one of the quadarant will suffice, but the technique for the playing
remains the same with a small difference.

![Cartesian system](cartesian_coordinates_2D.svg =350x)

The main difference between the real and computer world lies in an inverted 
y-axis. While in real world y-axis goes up (or what we think is "up" in earth).
Inside a computer, the y-axis is inverted. This is because the starting
position in the screen are set on the top left corner. So, the game takes
place in teh II quadarant (as seen in the above image) but with a positive 
y-axis.


## Random Positioning

To facilitate the game to the player, it helps to establish a game 
configuration right from the beginning. The player is able to do any 
modification with the starting setup or just play along inmediatly. That's 
up to her/him. The important fact is the game can be played in less than one 
or two actions.

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

I think the best way to move ships is to pick them up and drag them around. 
In principle this is done for PCs, but in addition, except by some minor 
differences should also work well for portable devices.

Like random positioning, ships cannot overlap other ships, nor leave the 
layout. Therefore, the same technique used to find random positions can be 
used to avoid overlapping and placing ships outside the grid: if the new 
position is negative, it acts like a wall.

To drag ships, perhaps it can be done with a shaded ship following the 
selected one. Another way can simply be fitting the dragged ship onto new box 
while moving. I still have to decide it what fits better.

Another critical point is to change orientation. I think the best approach is 
to click on the ship and if it is enough space then turn around clockwise. In 
case cannot be turned, a shaking animation with a buzzing sound should warn 
the player. Perhaps the animation can be accompanied by a color change. I 
think this kind of warning technique can be used in others forbidden 
situations: like banging with the grid borders or against another ship.

## Peer Communication

Because no server is desired for the gameplay, perhaps I can make use of the 
WebRTC data channel.

**Pending Research**.

## TODO
* [x] __[Modules]__: Extract Ship related functionality into its own module
* [ ] __[Random Positioning]__: prevent ships with the same orientation being placed in the 
same line when other ship is present.
* [ ] __[Custom Positioning]__: Dragging ships.
* [ ] __[Custom Positioning]__: Change ship orientation.
