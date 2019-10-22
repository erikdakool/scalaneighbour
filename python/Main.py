import schema_in_pb2 as proto

if __name__ == "__main__":

    boards = proto.PBoards()
    f = open("../ScalaAssignment/sample.bin", "rb")
    boards.ParseFromString(f.read())
    f.close()
    print(boards)
output = "puzzles " + str(len(boards.boards))
for board in boards.boards:
    output += "\n" + "size " + str(board.x) + "x" + str(board.x) + "\n"
    values = ""
    for value in board.squares:
        values += str(value)
    digitarray = list(values)
    line = 0
    for digit in digitarray:
        output += digit + " "
        line+=1
        if(line == board.x):
            output += "\n"
            line = 0
    output = output[:-1]
print(output)
