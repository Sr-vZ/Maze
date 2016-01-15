Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Maze))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Maze))==(Machine(Maze));
  Level(Machine(Maze))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Maze)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Maze))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Maze))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Maze))==(?);
  List_Includes(Machine(Maze))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Maze))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Maze))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Maze))==(?);
  Context_List_Variables(Machine(Maze))==(?);
  Abstract_List_Variables(Machine(Maze))==(?);
  Local_List_Variables(Machine(Maze))==(robotMovementHistory,robotMovement,yAxisPosition,xAxisPosition);
  List_Variables(Machine(Maze))==(robotMovementHistory,robotMovement,yAxisPosition,xAxisPosition);
  External_List_Variables(Machine(Maze))==(robotMovementHistory,robotMovement,yAxisPosition,xAxisPosition)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Maze))==(?);
  Abstract_List_VisibleVariables(Machine(Maze))==(?);
  External_List_VisibleVariables(Machine(Maze))==(?);
  Expanded_List_VisibleVariables(Machine(Maze))==(?);
  List_VisibleVariables(Machine(Maze))==(?);
  Internal_List_VisibleVariables(Machine(Maze))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Maze))==(btrue);
  Gluing_List_Invariant(Machine(Maze))==(btrue);
  Expanded_List_Invariant(Machine(Maze))==(btrue);
  Abstract_List_Invariant(Machine(Maze))==(btrue);
  Context_List_Invariant(Machine(Maze))==(btrue);
  List_Invariant(Machine(Maze))==(xAxisPosition: NATURAL1 & yAxisPosition: NATURAL1 & xAxisPosition: xAxis & yAxisPosition: yAxis & robotMovement: seq(MOVE) & robotMovementHistory: seq(INTEGER*INTEGER))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Maze))==(btrue);
  Abstract_List_Assertions(Machine(Maze))==(btrue);
  Context_List_Assertions(Machine(Maze))==(btrue);
  List_Assertions(Machine(Maze))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Maze))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Maze))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Maze))==(xAxisPosition,yAxisPosition,robotMovement,robotMovementHistory:=1,1,<>,<>);
  Context_List_Initialisation(Machine(Maze))==(skip);
  List_Initialisation(Machine(Maze))==(xAxisPosition:=1 || yAxisPosition:=1 || robotMovement:=<> || robotMovementHistory:=<>)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Maze))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Maze))==(btrue);
  List_Constraints(Machine(Maze))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Maze))==(moveN,moveE,moveS,moveW,moveToStart,coordinates,moveToRandom,exitReached,visitedSquares);
  List_Operations(Machine(Maze))==(moveN,moveE,moveS,moveW,moveToStart,coordinates,moveToRandom,exitReached,visitedSquares)
END
&
THEORY ListInputX IS
  List_Input(Machine(Maze),moveN)==(?);
  List_Input(Machine(Maze),moveE)==(?);
  List_Input(Machine(Maze),moveS)==(?);
  List_Input(Machine(Maze),moveW)==(?);
  List_Input(Machine(Maze),moveToStart)==(?);
  List_Input(Machine(Maze),coordinates)==(?);
  List_Input(Machine(Maze),moveToRandom)==(xx,yy);
  List_Input(Machine(Maze),exitReached)==(?);
  List_Input(Machine(Maze),visitedSquares)==(visitedx,visitedy)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Maze),moveN)==(move);
  List_Output(Machine(Maze),moveE)==(move);
  List_Output(Machine(Maze),moveS)==(move);
  List_Output(Machine(Maze),moveW)==(move);
  List_Output(Machine(Maze),moveToStart)==(move);
  List_Output(Machine(Maze),coordinates)==(robotPosition);
  List_Output(Machine(Maze),moveToRandom)==(TP);
  List_Output(Machine(Maze),exitReached)==(exitSquare);
  List_Output(Machine(Maze),visitedSquares)==(visited)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Maze),moveN)==(move <-- moveN);
  List_Header(Machine(Maze),moveE)==(move <-- moveE);
  List_Header(Machine(Maze),moveS)==(move <-- moveS);
  List_Header(Machine(Maze),moveW)==(move <-- moveW);
  List_Header(Machine(Maze),moveToStart)==(move <-- moveToStart);
  List_Header(Machine(Maze),coordinates)==(robotPosition <-- coordinates);
  List_Header(Machine(Maze),moveToRandom)==(TP <-- moveToRandom(xx,yy));
  List_Header(Machine(Maze),exitReached)==(exitSquare <-- exitReached);
  List_Header(Machine(Maze),visitedSquares)==(visited <-- visitedSquares(visitedx,visitedy))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Maze),moveN)==(btrue);
  List_Precondition(Machine(Maze),moveE)==(btrue);
  List_Precondition(Machine(Maze),moveS)==(btrue);
  List_Precondition(Machine(Maze),moveW)==(btrue);
  List_Precondition(Machine(Maze),moveToStart)==(btrue);
  List_Precondition(Machine(Maze),coordinates)==(btrue);
  List_Precondition(Machine(Maze),moveToRandom)==(xx: xAxis & yy: yAxis);
  List_Precondition(Machine(Maze),exitReached)==(btrue);
  List_Precondition(Machine(Maze),visitedSquares)==(visitedx: xAxis & visitedy: yAxis)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Maze),visitedSquares)==(visitedx: xAxis & visitedy: yAxis | visitedx|->visitedy: ran(robotMovementHistory) ==> visited:=yes [] not(visitedx|->visitedy: ran(robotMovementHistory)) ==> visited:=no);
  Expanded_List_Substitution(Machine(Maze),exitReached)==(btrue | xAxisPosition|->yAxisPosition = 1|->5 ==> exitSquare:=exitSquareReached [] not(xAxisPosition|->yAxisPosition = 1|->5) ==> exitSquare:=exitSquareNotReached);
  Expanded_List_Substitution(Machine(Maze),moveToRandom)==(xx: xAxis & yy: yAxis | xx|->yy/:blackSquare ==> xAxisPosition,yAxisPosition,TP:=xx,yy,moveToRandomSquare [] not(xx|->yy/:blackSquare) ==> skip);
  Expanded_List_Substitution(Machine(Maze),coordinates)==(btrue | xAxisPosition>=1 or yAxisPosition>=1 ==> robotPosition:=xAxisPosition|->yAxisPosition [] not(xAxisPosition>=1 or yAxisPosition>=1) ==> skip);
  Expanded_List_Substitution(Machine(Maze),moveToStart)==(btrue | xAxisPosition>=1 or yAxisPosition>=1 ==> xAxisPosition,yAxisPosition,move:=1,1,moveToEntrySquare [] not(xAxisPosition>=1 or yAxisPosition>=1) ==> skip);
  Expanded_List_Substitution(Machine(Maze),moveW)==(btrue | xAxisPosition|->yAxisPosition/:firstColumn & xAxisPosition-1|->yAxisPosition/:blackSquare ==> xAxisPosition,robotMovement,robotMovementHistory,move:=xAxisPosition-1,robotMovement<-moveWest,robotMovementHistory<-(xAxisPosition|->yAxisPosition),moveWest [] not(xAxisPosition|->yAxisPosition/:firstColumn & xAxisPosition-1|->yAxisPosition/:blackSquare) ==> skip);
  Expanded_List_Substitution(Machine(Maze),moveS)==(btrue | xAxisPosition|->yAxisPosition/:firstRow & xAxisPosition|->yAxisPosition-1/:blackSquare ==> yAxisPosition,robotMovement,robotMovementHistory,move:=yAxisPosition-1,robotMovement<-moveSouth,robotMovementHistory<-(xAxisPosition|->yAxisPosition),moveSouth [] not(xAxisPosition|->yAxisPosition/:firstRow & xAxisPosition|->yAxisPosition-1/:blackSquare) ==> skip);
  Expanded_List_Substitution(Machine(Maze),moveE)==(btrue | xAxisPosition|->yAxisPosition/:lastColumn & xAxisPosition+1|->yAxisPosition/:blackSquare ==> xAxisPosition,robotMovement,robotMovementHistory,move:=xAxisPosition+1,robotMovement<-moveEast,robotMovementHistory<-(xAxisPosition|->yAxisPosition),moveEast [] not(xAxisPosition|->yAxisPosition/:lastColumn & xAxisPosition+1|->yAxisPosition/:blackSquare) ==> skip);
  Expanded_List_Substitution(Machine(Maze),moveN)==(btrue | xAxisPosition|->yAxisPosition/:lastRow & xAxisPosition|->yAxisPosition+1/:blackSquare ==> yAxisPosition,robotMovement,robotMovementHistory,move:=yAxisPosition+1,robotMovement<-moveNorth,robotMovementHistory<-(xAxisPosition|->yAxisPosition),moveNorth [] not(xAxisPosition|->yAxisPosition/:lastRow & xAxisPosition|->yAxisPosition+1/:blackSquare) ==> skip);
  List_Substitution(Machine(Maze),moveN)==(IF xAxisPosition|->yAxisPosition/:lastRow & xAxisPosition|->yAxisPosition+1/:blackSquare THEN yAxisPosition:=yAxisPosition+1 || robotMovement:=robotMovement<-moveNorth || robotMovementHistory:=robotMovementHistory<-(xAxisPosition|->yAxisPosition) || move:=moveNorth END);
  List_Substitution(Machine(Maze),moveE)==(IF xAxisPosition|->yAxisPosition/:lastColumn & xAxisPosition+1|->yAxisPosition/:blackSquare THEN xAxisPosition:=xAxisPosition+1 || robotMovement:=robotMovement<-moveEast || robotMovementHistory:=robotMovementHistory<-(xAxisPosition|->yAxisPosition) || move:=moveEast END);
  List_Substitution(Machine(Maze),moveS)==(IF xAxisPosition|->yAxisPosition/:firstRow & xAxisPosition|->yAxisPosition-1/:blackSquare THEN yAxisPosition:=yAxisPosition-1 || robotMovement:=robotMovement<-moveSouth || robotMovementHistory:=robotMovementHistory<-(xAxisPosition|->yAxisPosition) || move:=moveSouth END);
  List_Substitution(Machine(Maze),moveW)==(IF xAxisPosition|->yAxisPosition/:firstColumn & xAxisPosition-1|->yAxisPosition/:blackSquare THEN xAxisPosition:=xAxisPosition-1 || robotMovement:=robotMovement<-moveWest || robotMovementHistory:=robotMovementHistory<-(xAxisPosition|->yAxisPosition) || move:=moveWest END);
  List_Substitution(Machine(Maze),moveToStart)==(IF xAxisPosition>=1 or yAxisPosition>=1 THEN xAxisPosition:=1 || yAxisPosition:=1 || move:=moveToEntrySquare END);
  List_Substitution(Machine(Maze),coordinates)==(IF xAxisPosition>=1 or yAxisPosition>=1 THEN robotPosition:=xAxisPosition|->yAxisPosition END);
  List_Substitution(Machine(Maze),moveToRandom)==(IF xx|->yy/:blackSquare THEN xAxisPosition:=xx || yAxisPosition:=yy || TP:=moveToRandomSquare END);
  List_Substitution(Machine(Maze),exitReached)==(IF xAxisPosition|->yAxisPosition = 1|->5 THEN exitSquare:=exitSquareReached ELSE exitSquare:=exitSquareNotReached END);
  List_Substitution(Machine(Maze),visitedSquares)==(IF visitedx|->visitedy: ran(robotMovementHistory) THEN visited:=yes ELSE visited:=no END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Maze))==(xAxis,yAxis,maze,firstColumn,lastColumn,firstRow,lastRow,blackSquare);
  Inherited_List_Constants(Machine(Maze))==(?);
  List_Constants(Machine(Maze))==(xAxis,yAxis,maze,firstColumn,lastColumn,firstRow,lastRow,blackSquare)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Maze),MOVE)==({moveNorth,moveEast,moveSouth,moveWest,moveToEntrySquare,exitSquareReached,exitSquareNotReached,moveToRandomSquare});
  Context_List_Enumerated(Machine(Maze))==(?);
  Context_List_Defered(Machine(Maze))==(?);
  Context_List_Sets(Machine(Maze))==(?);
  List_Valuable_Sets(Machine(Maze))==(?);
  Inherited_List_Enumerated(Machine(Maze))==(?);
  Inherited_List_Defered(Machine(Maze))==(?);
  Inherited_List_Sets(Machine(Maze))==(?);
  List_Enumerated(Machine(Maze))==(MOVE,EXITSQUARE);
  List_Defered(Machine(Maze))==(?);
  List_Sets(Machine(Maze))==(MOVE,EXITSQUARE);
  Set_Definition(Machine(Maze),EXITSQUARE)==({yes,no})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Maze))==(?);
  Expanded_List_HiddenConstants(Machine(Maze))==(?);
  List_HiddenConstants(Machine(Maze))==(?);
  External_List_HiddenConstants(Machine(Maze))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Maze))==(btrue);
  Context_List_Properties(Machine(Maze))==(btrue);
  Inherited_List_Properties(Machine(Maze))==(btrue);
  List_Properties(Machine(Maze))==(xAxis <: NATURAL1 & yAxis <: NATURAL1 & xAxis = 1..7 & yAxis = 1..5 & maze: NATURAL1 <-> NATURAL1 & maze = xAxis*yAxis & firstColumn: NATURAL1 <-> NATURAL1 & lastColumn: NATURAL1 <-> NATURAL1 & firstColumn = {1}<|maze & lastColumn = {7}<|maze & firstRow: NATURAL1 <-> NATURAL1 & lastRow: NATURAL1 <-> NATURAL1 & firstRow = maze|>{1} & lastRow = maze|>{5} & blackSquare: NATURAL1 <-> NATURAL1 & blackSquare = {1|->3,2|->1,2|->3,2|->5,3|->3,4|->2,4|->3,4|->4,6|->1,6|->2,6|->4,7|->4} & MOVE: FIN(INTEGER) & not(MOVE = {}) & EXITSQUARE: FIN(INTEGER) & not(EXITSQUARE = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Maze),moveN)==(?);
  List_ANY_Var(Machine(Maze),moveE)==(?);
  List_ANY_Var(Machine(Maze),moveS)==(?);
  List_ANY_Var(Machine(Maze),moveW)==(?);
  List_ANY_Var(Machine(Maze),moveToStart)==(?);
  List_ANY_Var(Machine(Maze),coordinates)==(?);
  List_ANY_Var(Machine(Maze),moveToRandom)==(?);
  List_ANY_Var(Machine(Maze),exitReached)==(?);
  List_ANY_Var(Machine(Maze),visitedSquares)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Maze)) == (xAxis,yAxis,maze,firstColumn,lastColumn,firstRow,lastRow,blackSquare,MOVE,EXITSQUARE,moveNorth,moveEast,moveSouth,moveWest,moveToEntrySquare,exitSquareReached,exitSquareNotReached,moveToRandomSquare,yes,no | ? | robotMovementHistory,robotMovement,yAxisPosition,xAxisPosition | ? | moveN,moveE,moveS,moveW,moveToStart,coordinates,moveToRandom,exitReached,visitedSquares | ? | ? | ? | Maze);
  List_Of_HiddenCst_Ids(Machine(Maze)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Maze)) == (xAxis,yAxis,maze,firstColumn,lastColumn,firstRow,lastRow,blackSquare);
  List_Of_VisibleVar_Ids(Machine(Maze)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Maze)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Maze)) == (Type(MOVE) == Cst(SetOf(etype(MOVE,0,7)));Type(EXITSQUARE) == Cst(SetOf(etype(EXITSQUARE,0,1))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Maze)) == (Type(moveNorth) == Cst(etype(MOVE,0,7));Type(moveEast) == Cst(etype(MOVE,0,7));Type(moveSouth) == Cst(etype(MOVE,0,7));Type(moveWest) == Cst(etype(MOVE,0,7));Type(moveToEntrySquare) == Cst(etype(MOVE,0,7));Type(exitSquareReached) == Cst(etype(MOVE,0,7));Type(exitSquareNotReached) == Cst(etype(MOVE,0,7));Type(moveToRandomSquare) == Cst(etype(MOVE,0,7));Type(yes) == Cst(etype(EXITSQUARE,0,1));Type(no) == Cst(etype(EXITSQUARE,0,1));Type(xAxis) == Cst(SetOf(btype(INTEGER,"[xAxis","]xAxis")));Type(yAxis) == Cst(SetOf(btype(INTEGER,"[yAxis","]yAxis")));Type(maze) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(firstColumn) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(lastColumn) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(firstRow) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(lastRow) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(blackSquare) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Maze)) == (Type(robotMovementHistory) == Mvl(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(robotMovement) == Mvl(SetOf(btype(INTEGER,?,?)*etype(MOVE,?,?)));Type(yAxisPosition) == Mvl(btype(INTEGER,?,?));Type(xAxisPosition) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Maze)) == (Type(visitedSquares) == Cst(etype(EXITSQUARE,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(exitReached) == Cst(etype(MOVE,?,?),No_type);Type(moveToRandom) == Cst(etype(MOVE,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(coordinates) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(moveToStart) == Cst(etype(MOVE,?,?),No_type);Type(moveW) == Cst(etype(MOVE,?,?),No_type);Type(moveS) == Cst(etype(MOVE,?,?),No_type);Type(moveE) == Cst(etype(MOVE,?,?),No_type);Type(moveN) == Cst(etype(MOVE,?,?),No_type));
  Observers(Machine(Maze)) == (Type(visitedSquares) == Cst(etype(EXITSQUARE,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(exitReached) == Cst(etype(MOVE,?,?),No_type);Type(coordinates) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
