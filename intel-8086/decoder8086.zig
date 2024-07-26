const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;

const DecoderError = error{ ExpectedArgument, InvalidByteLength };

const opcode_mask: u8 = 0b1111_1100;
const direction_mask: u8 = 0b0000_0010;
const word_mask: u8 = 0b0000_0001;
const mode_mask: u8 = 0b1100_0000;
const operand1_mask: u8 = 0b0011_1000;
const operand2_mask: u8 = 0b0000_0111;

const Instruction = struct {
    opcode: u6,
    direction: u1,
    word: u1,
    mode: u2,
    operand1: u3,
    operand2: u3,

    pub fn print(self: Instruction) void {
        const operation = switch (self.opcode) {
            0b100010 => "mov",
            else => "unrecognized",
        };

        const op1 = getRegisterName(self.operand1, self.word);
        const op2 = getRegisterName(self.operand2, self.word);

        const print_order = if (self.direction == 0) .{ operation, op2, op1 } else .{ operation, op1, op2 };
        std.debug.print("{s} {s}, {s}\n", print_order);
    }

    fn getRegisterName(operand: u3, word: u1) *const [2:0]u8 {
        return switch (operand) {
            0b000 => if (word == 0) "al" else "ax",
            0b001 => if (word == 0) "cl" else "cx",
            0b010 => if (word == 0) "dl" else "dx",
            0b011 => if (word == 0) "bl" else "bx",
            0b100 => if (word == 0) "ah" else "sp",
            0b101 => if (word == 0) "ch" else "bp",
            0b110 => if (word == 0) "dh" else "si",
            0b111 => if (word == 0) "bh" else "di",
        };
    }
};

pub fn CreateInstruction(bytes: []u8) !Instruction {
    if (bytes.len != 2) return DecoderError.InvalidByteLength;

    return Instruction{ .opcode = @intCast((bytes.ptr[0] & opcode_mask) >> 2), .direction = @intCast((bytes.ptr[0] & direction_mask) >> 1), .word = @intCast(bytes.ptr[0] & word_mask), .mode = @intCast((bytes.ptr[1] & mode_mask) >> 6), .operand1 = @intCast((bytes.ptr[1] & operand1_mask) >> 3), .operand2 = @intCast(bytes.ptr[1] & operand2_mask) };
}

pub fn main() !void {
    // Get Allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) return error.ExpectedArgument;

    const cwd = std.fs.cwd();

    var buffer: []u8 = try allocator.alloc(u8, 4096);
    defer allocator.free(buffer);

    for (1..args.len) |i| {
        const file = try cwd.openFile(args[i], .{});
        defer file.close();

        const bytes_read = try file.read(buffer);
        var byte_index: usize = 0;

        print("{s}\nbits 16\n\n", .{args[i]});

        while (byte_index < bytes_read) : (byte_index += 2) {
            var instr: Instruction = try CreateInstruction(buffer[byte_index .. byte_index + 2]);
            instr.print();
        }

        print("\n", .{});
    }
}
