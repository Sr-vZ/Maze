/* Maze
 * Author: Anique
 * Creation date: 06/01/2016
 */
MACHINE
    Maze
    
SETS
    
    MOVE = {moveNorth, moveEast, moveSouth, moveWest, moveToEntrySquare, exitSquareReached, exitSquareNotReached, moveToRandomSquare};
	
	OUTPUT = {errorInternalWall, errorExternalWall, moveSuccessful, moveUnSuccessful};
    
    EXITSQUARE = {yes, no}

CONSTANTS
    
    xAxis, yAxis, maze, firstColumn, lastColumn, firstRow, lastRow, blackSquare
    
PROPERTIES
    
    xAxis <: NATURAL1 &
    yAxis <: NATURAL1 &
    xAxis = 1..7 &
    yAxis = 1..5 &
    maze : NATURAL1 <-> NATURAL1 &
    maze = xAxis * yAxis &
    firstColumn : NATURAL1 <-> NATURAL1 &
    lastColumn : NATURAL1 <-> NATURAL1 &
    firstColumn = {1} <| maze &
    lastColumn = {7} <| maze &
    firstRow : NATURAL1 <-> NATURAL1 &
    lastRow : NATURAL1 <-> NATURAL1 &
    firstRow = maze |> {1} &
    lastRow = maze |> {5} &
    blackSquare : NATURAL1 <-> NATURAL1 &
    blackSquare = {(1|->3), (2|->1), (2|->3), (2|->5), (3 |->3), (4|->2), (4|->3), (4|->4), (6|->1), (6|->2), (6|->4), (7|->4)}
    
VARIABLES
    
    xAxisPosition, yAxisPosition, robotMovement, robotMovementHistory
    
INVARIANT
    
    xAxisPosition : NATURAL1 &
    yAxisPosition : NATURAL1 &
    xAxisPosition : xAxis &
    yAxisPosition : yAxis &
    robotMovement : seq(MOVE) &
    robotMovementHistory : seq(INTEGER * INTEGER)
    
INITIALISATION
    
    xAxisPosition := 1 ||
    yAxisPosition := 1 ||
    robotMovement := [] ||
    robotMovementHistory := []
    
OPERATIONS
    
    move <-- moveN =
    IF
        ((xAxisPosition|->yAxisPosition) /: lastRow & 
        (xAxisPosition|->yAxisPosition + 1) /: blackSquare)
    THEN
        yAxisPosition := yAxisPosition + 1 ||
        robotMovement := robotMovement <- moveNorth ||
        robotMovementHistory := robotMovementHistory <- (xAxisPosition|->yAxisPosition) ||
        move := moveSuccessful
	    ELSIF((xAxisPosition|->yAxisPosition):lastRow)
	      THEN move :=  errorExternalWall
	          ELSE move := errorInternalWall
    END;
    move <-- moveE =
    IF
        ((xAxisPosition|->yAxisPosition) /: lastColumn & 
        (xAxisPosition + 1|->yAxisPosition) /: blackSquare)
    THEN
        xAxisPosition := xAxisPosition + 1 ||
        robotMovement := robotMovement <- moveEast ||
        robotMovementHistory := robotMovementHistory <- (xAxisPosition|->yAxisPosition) ||
        move := moveSuccessful
	    ELSIF((xAxisPosition|->yAxisPosition):lastColumn)
	      THEN move :=  errorExternalWall
	          ELSE move := errorInternalWall
    END;
    move <-- moveS =
    IF
        ((xAxisPosition|->yAxisPosition) /: firstRow & 
        (xAxisPosition|->yAxisPosition - 1) /: blackSquare)
    THEN
        yAxisPosition := yAxisPosition - 1 ||
        robotMovement := robotMovement <- moveSouth ||
        robotMovementHistory := robotMovementHistory <- (xAxisPosition|->yAxisPosition) ||
        move := moveSuccessful
	    ELSIF((xAxisPosition|->yAxisPosition):firstRow)
	      THEN move :=  errorExternalWall
	          ELSE move := errorInternalWall
    END;
    move <-- moveW =
    IF
        ((xAxisPosition|->yAxisPosition) /: firstColumn & 
        (xAxisPosition - 1|->yAxisPosition) /: blackSquare)
    THEN
        xAxisPosition := xAxisPosition - 1 ||
        robotMovement := robotMovement <- moveWest ||
        robotMovementHistory := robotMovementHistory <- (xAxisPosition|->yAxisPosition) ||
        move := moveSuccessful
	    ELSIF((xAxisPosition|->yAxisPosition):firstColumn)
	      THEN move :=  errorExternalWall
	          ELSE move := errorInternalWall
    END;
    move <-- moveToStart =
    IF
        (xAxisPosition >= 1 or yAxisPosition >= 1)
    THEN
        xAxisPosition := 1 ||
        yAxisPosition := 1 ||
        move := moveToEntrySquare
    END;
    robotPosition <-- coordinates =
    IF
        (xAxisPosition >= 1 or yAxisPosition >= 1)
    THEN
        robotPosition := (xAxisPosition|->yAxisPosition)
    END;
    TP <-- moveToRandom(xx,yy) =
    PRE
        xx : xAxis &
        yy : yAxis
    THEN
        IF
            (xx|->yy) /: blackSquare & (xx|->yy) : maze
        THEN
            xAxisPosition := xx ||
            yAxisPosition := yy ||
            TP := moveSuccessful
			ELSIF((xx|->yy) /: blackSquare )
			   THEN TP :=  errorInternalWall
			       ELSE TP := moveUnSuccessful
        END
    END;
    exitSquare <-- exitReached =
    IF
        (xAxisPosition|->yAxisPosition) = (1|->5)
    THEN
        exitSquare := exitSquareReached
    ELSE
        exitSquare := exitSquareNotReached
    END;
    visited <-- visitedSquares(visitedx, visitedy) =
    PRE
        visitedx : xAxis &
        visitedy : yAxis
    THEN
        IF
            (visitedx|->visitedy : ran(robotMovementHistory))
        THEN
            visited := yes
        ELSE
            visited := no
        END
    END;
    
	squaresequence <--  robotMovements =
    BEGIN
    	squaresequence:=  robotMovement
    END;

    movementsequence <-- robotMovementHistorys =
    BEGIN 
    	movementsequence:= robotMovementHistory
    END
END
