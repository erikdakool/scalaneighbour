import schema_in_pb2


def txtToBinaryProto():

    lines = [line.rstrip('\n') for line in open("puzzle_unsolved.txt")]

    numberOfPuzzles = int(lines[0][-1])
    print("Puzzle count: ", numberOfPuzzles)

    getPuzzleSizes = []
    for i in lines:
        if i.startswith("size"):
            getPuzzleSizes.append(i)
    print(getPuzzleSizes)

    lineIndexCounter = 2

    ListOfMatrix = schema_in_pb2.PPuzzles()

    for p in range(numberOfPuzzles):
        puzzleDimension = int(getPuzzleSizes[p][-1])

        puzzleYLength = puzzleDimension + puzzleDimension - 1

        print("Index:", lineIndexCounter)
        print("Puzzle:", p, "Dim:", puzzleDimension, "line:", puzzleYLength)

        matrix = schema_in_pb2.PPuzzle()

        for y in range(puzzleDimension):

            for x in range(0, puzzleDimension):

                square = schema_in_pb2.PSquare()
                directions = schema_in_pb2.PSquare.Directions

                squareValue = lines[lineIndexCounter + y*2][x*4]
                print(squareValue)
                if x > 0:
                    print("| left" + lines[lineIndexCounter + y*2][(x*4)-2])
                    if lines[lineIndexCounter + y*2][(x*4)-2] == 'x':
                        square.neighbour.append(directions.Left)

                if y > 0:
                    print("| up" + lines[lineIndexCounter + y*2-1][(x*4)])
                    if lines[lineIndexCounter + y*2-1][(x*4)] == 'x':
                        square.neighbour.append(directions.Up)
                if squareValue.isdigit():
                    print("this square is solved: ", squareValue)
                    square.value.append(int(squareValue))
                    #square.solved = True

                #currentXValues.elements.append(square)

                matrix.square.append(square)
        lineIndexCounter += puzzleYLength + 1
        ListOfMatrix.puzzles.append(matrix)

    createBin = open("puzzle_unsolved.bin", 'wb')
    createBin.write(ListOfMatrix.SerializeToString())
    createBin.close()

    readBin = open("puzzle_unsolved.bin", 'rb')
    test = schema_in_pb2.PPuzzles()
    test.ParseFromString(readBin.read())
    print(test)
    readBin.close()


if __name__ == '__main__':
    txtToBinaryProto()
