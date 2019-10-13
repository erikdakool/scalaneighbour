import schema_in_pb2 as proto

if __name__ == "__main__":
    test = proto.Square()
    test.x = 12
    board = proto.Board()
    board.x = 12
    test2 = board.squares.add()
    test2.x = 14

    print(board)

with open("simple.bin", "wb") as f:
    print("write as binary")
    bytesAsString = test.SerializeToString()
    f.write(bytesAsString)

with open("simple.bin", "rb") as f:
    print("read values")
    simple_message_read = proto.Square().FromString(f.read())

print(simple_message_read)