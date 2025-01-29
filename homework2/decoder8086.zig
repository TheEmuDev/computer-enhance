const std = @import("std");
const assert = std.debug.assert;
const fmt = std.fmt;
const log = std.log;

const DecoderError = error{ ExpectedArgument, InvalidByteLength, MalformedInstruction, NotImplemented, UnknownOpCode };

const OperationType = enum { opcode4, opcode6, opcode7, opcode8 };
const operationcode = union(OperationType) { opcode4: u4, opcode6: u6, opcode7: u7, opcode8: u8 };

const Instruction = struct {
    opcode: operationcode,
    direction: ?u1,
    word: ?u1,
    mode: ?u2,
    register: ?u3,
    reg_mem: ?u3,
    disp_low: ?u8,
    disp_high: ?u8,
    data_low: ?u8,
    data_high: ?u8,
    segment_reg: ?u2,
    size: usize,

    pub fn init(opcode: operationcode, d: ?u1, w: ?u1, mod: ?u2, reg: ?u3, reg_mem: ?u3, disp_lo: ?u8, disp_hi: ?u8, data_lo: ?u8, data_hi: ?u8, sr: ?u2, size: usize) Instruction {
        return Instruction{ .opcode = opcode, .direction = d, .word = w, .mode = mod, .register = reg, .reg_mem = reg_mem, .disp_low = disp_lo, .disp_high = disp_hi, .data_low = data_lo, .data_high = data_hi, .segment_reg = sr, .size = size };
    }

    // mov cx, 12
    // mov cx, dx
    // mov cx, [bx + si]
    // mov cx, [bx + si + 4]
    // mov [bx + si], cx

    pub fn decode(self: Instruction, buf: []u8) ![]u8 {
        var msg: []u8 = undefined;
        var operand1: []const u8 = undefined;
        var operand2: []const u8 = undefined;
        var data: u16 = undefined;
        var disp: u16 = undefined;

        switch (self.opcode) {
            OperationType.opcode4 => |opcode| {
                switch (opcode) {
                    0b1011 => { // MOV: immediate to register/memory
                        const data_low = self.data_low orelse return DecoderError.MalformedInstruction;
                        const register = self.register orelse return DecoderError.MalformedInstruction;
                        const word = self.word orelse return DecoderError.MalformedInstruction;

                        if (word == 0b1 and self.data_high == null) {
                            return DecoderError.MalformedInstruction;
                        }

                        operand1 = decodeRegister(register, word);

                        data = switch (word) {
                            0b0 => data_low,
                            0b1 => getCombined(data_low, self.data_high),
                        };

                        msg = try fmt.bufPrint(buf, "mov {s}, {d}\n", .{ operand1, data });
                    },
                    else => {
                        log.info("Opcode 4: {b}", .{opcode});
                        return DecoderError.UnknownOpCode;
                    },
                }
            },
            OperationType.opcode6 => |opcode| {
                switch (opcode) {
                    0b1000_10 => { // MOV: Register/Memory to/from Register
                        const direction = self.direction orelse return DecoderError.MalformedInstruction;
                        const word = self.word orelse return DecoderError.MalformedInstruction;
                        const register = self.register orelse return DecoderError.MalformedInstruction;
                        const reg_mem = self.reg_mem orelse return DecoderError.MalformedInstruction;
                        const mode = self.mode orelse return DecoderError.MalformedInstruction;

                        if ((mode == 0b01 or mode == 0b10) and self.disp_low == null) {
                            return DecoderError.MalformedInstruction;
                        }

                        if (mode == 0b10 and self.disp_high == null) {
                            return DecoderError.MalformedInstruction;
                        }

                        operand1 = if (direction == 0b0) decodeRegMem(reg_mem, mode, word) else decodeRegister(register, word);
                        operand2 = if (direction == 0b0) decodeRegister(register, word) else decodeRegMem(reg_mem, mode, word);

                        if ((mode == 0b00 and reg_mem == 0b110) or mode == 0b01 or mode == 0b10) {
                            disp = getCombined(self.disp_low, self.disp_high);
                        }

                        if (direction == 0b0) {
                            if (mode == 0b00 or mode == 0b11) {
                                msg = if (mode == 0b00 and reg_mem == 0b110) try fmt.bufPrint(buf, "mov [{d}], {s}\n", .{ disp, operand2 }) else try fmt.bufPrint(buf, "mov {s}, {s}\n", .{ operand1, operand2 });
                            } else {
                                msg = try fmt.bufPrint(buf, "mov [{s}{d}], {s}\n", .{ operand1, disp, operand2 });
                            }
                        } else {
                            if (mode == 0b00 or mode == 0b11) {
                                msg = if (mode == 0b00 and reg_mem == 0b110) try fmt.bufPrint(buf, "mov {s}, [{d}]\n", .{ operand1, disp }) else try fmt.bufPrint(buf, "mov {s}, {s}\n", .{ operand1, operand2 });
                            } else {
                                msg = try fmt.bufPrint(buf, "mov {s}, [{s}{d}]\n", .{ operand1, operand2, disp });
                            }
                        }
                    },
                    else => {
                        log.info("Opcode 6: {b}", .{opcode});
                        return DecoderError.UnknownOpCode;
                    },
                }
            },
            OperationType.opcode7 => |opcode| {
                switch (opcode) {
                    0b1100_011 => { // MOV: Immediate to Register/Memory
                        const word = self.word orelse return DecoderError.MalformedInstruction;
                        const mode = self.mode orelse return DecoderError.MalformedInstruction;
                        const reg_mem = self.reg_mem orelse return DecoderError.MalformedInstruction;
                        const data_low = self.data_low orelse return DecoderError.MalformedInstruction;

                        if (word == 0b1 and self.data_high == null) {
                            return DecoderError.MalformedInstruction;
                        }

                        if (mode == 0b01 and self.disp_low == null) {
                            return DecoderError.MalformedInstruction;
                        }

                        if (mode == 0b10 and (self.disp_low == null or self.disp_high == null)) {
                            return DecoderError.MalformedInstruction;
                        }

                        if (mode == 0b00 or mode == 0b11) {
                            if (mode == 0b00 and reg_mem == 0b110) {
                                disp = getCombined(self.disp_low, self.disp_high);
                            } else {
                                operand1 = decodeRegMem(reg_mem, mode, word);
                            }
                        } else {
                            operand1 = decodeRegMem(reg_mem, mode, word);
                            disp = getCombined(self.disp_low, self.disp_high);
                        }

                        data = if (word == 0b0) data_low else getCombined(data_low, self.data_high);

                        switch (mode) {
                            0b00, 0b11 => {
                                if (mode == 0b00 and reg_mem == 0b110) {
                                    msg = try fmt.bufPrint(buf, "mov [{d}], {d}", .{ disp, data });
                                } else msg = try fmt.bufPrint(buf, "mov {s}, {d}\n", .{ operand1, data });
                            },
                            0b01, 0b10 => {
                                msg = try fmt.bufPrint(buf, "mov [{s}{d}], {d}\n", .{ operand1, disp, data });
                            },
                        }
                    },
                    0b1010_000 => { // MOV: Memory to Accumulator
                        data = getCombined(self.data_low, self.data_high);
                        msg = try fmt.bufPrint(buf, "mov ax, [{d}]\n", .{data});
                    },
                    0b1010_001 => { // MOV: Accumulator to Memory
                        data = getCombined(self.data_low, self.data_high);
                        msg = try fmt.bufPrint(buf, "mov [{d}], ax\n", .{data});
                    },
                    else => {
                        log.info("Opcode 7: {b}", .{opcode});
                        return DecoderError.UnknownOpCode;
                    },
                }
            },
            OperationType.opcode8 => |opcode| {
                switch (opcode) {
                    0b1000_1110 => { // Register/memory to segment register
                        return DecoderError.NotImplemented;
                    },
                    0b1000_1100 => { // Segment register to register/memory
                        return DecoderError.NotImplemented;
                    },
                    else => {
                        log.info("Opcode 8: {b}", .{opcode});
                        return DecoderError.UnknownOpCode;
                    },
                }
            },
        }

        return msg;
    }

    pub fn DebugPrint(self: Instruction) void {
        log.debug("Instruction----------------", .{});
        log.debug("opcode: {any}", .{self.opcode});
        log.debug("direction: {?b}", .{self.direction});
        log.debug("word: {?b}", .{self.word});
        log.debug("mode: {?b}", .{self.mode});
        log.debug("register: {?b}", .{self.register});
        log.debug("reg_mem: {?b}", .{self.reg_mem});
        log.debug("disp_low: {?b}", .{self.disp_low});
        log.debug("disp_high: {?b}", .{self.disp_high});
        log.debug("data_low: {?b}", .{self.data_low});
        log.debug("data_high: {?b}", .{self.data_high});
        log.debug("seg_reg: {?b}", .{self.segment_reg});
        log.debug("size: {d}", .{self.size});
        log.debug("---------------------------", .{});
    }

    fn decodeRegister(reg: u3, w: u1) []const u8 {
        return switch (reg) {
            0b000 => if (w == 0b0) "al" else "ax",
            0b001 => if (w == 0b0) "cl" else "cx",
            0b010 => if (w == 0b0) "dl" else "dx",
            0b011 => if (w == 0b0) "bl" else "bx",
            0b100 => if (w == 0b0) "ah" else "sp",
            0b101 => if (w == 0b0) "ch" else "bp",
            0b110 => if (w == 0b0) "dh" else "si",
            0b111 => if (w == 0b0) "bh" else "di",
        };
    }

    fn decodeRegMem(reg_mem: u3, mod: u2, w: u1) []const u8 {
        switch (mod) {
            0b00 => { // Memory Mode, no displacement follows except when R/M = 0b110, then 16-bit displacement follows
                return switch (reg_mem) {
                    0b000 => "[bx + si]",
                    0b001 => "[bx + di]",
                    0b010 => "[bp + si]",
                    0b011 => "[bp + di]",
                    0b100 => "[si]",
                    0b101 => "[di]",
                    0b110 => "[{d}]", // 16-bit displacement
                    0b111 => "[bx]",
                };
            },
            0b01, 0b10 => { // Memory Mode, 8/16-bit displacement follows
                return switch (reg_mem) {
                    0b000 => "bx + si + ",
                    0b001 => "bx + di + ",
                    0b010 => "bp + si + ",
                    0b011 => "bp + di + ",
                    0b100 => "si + ",
                    0b101 => "di + ",
                    0b110 => "bp + ",
                    0b111 => "bx + ",
                };
            },
            0b11 => { // Register Mode, no displacement
                return switch (reg_mem) {
                    0b000 => if (w == 0b0) "al" else "ax",
                    0b001 => if (w == 0b0) "cl" else "cx",
                    0b010 => if (w == 0b0) "dl" else "dx",
                    0b011 => if (w == 0b0) "bl" else "bx",
                    0b100 => if (w == 0b0) "ah" else "sp",
                    0b101 => if (w == 0b0) "ch" else "bp",
                    0b110 => if (w == 0b0) "dh" else "si",
                    0b111 => if (w == 0b0) "bh" else "di",
                };
            },
        }
    }

    fn getCombined(low: ?u8, high: ?u8) u16 {
        var combined: u16 = 0b0000_0000_0000_0000;
        const lo = low orelse 0b0000_0000;
        const hi = high orelse 0b0000_00000;

        combined = (combined | hi) << 8;
        combined = combined | lo;

        return combined;
    }
};

pub fn CreateInstruction(bytes: []u8) !Instruction {
    if (bytes.len < 2) {
        return DecoderError.InvalidByteLength;
    }

    const opcode_mask_4: u8 = 0b1111_0000;
    const opcode_mask_6: u8 = 0b1111_1100;
    const opcode_mask_7: u8 = 0b1111_1110;

    var opcode: operationcode = undefined;
    var direction: ?u1 = undefined;
    var word: ?u1 = undefined;
    var mode: ?u2 = undefined;
    var reg: ?u3 = undefined;
    var reg_mem: ?u3 = undefined;
    var data_low: ?u8 = undefined;
    var data_high: ?u8 = undefined;
    var displacement_low: ?u8 = undefined;
    var displacement_high: ?u8 = undefined;
    var segment_reg: ?u2 = undefined;
    var size: usize = undefined;

    const opcode4: u4 = @intCast((bytes[0] & opcode_mask_4) >> 4);

    if (opcode4 == 0b1011) { // immediate to register
        const instruct4_word_mask: u8 = 0b0000_1000;
        const instruct4_reg_mask: u8 = 0b0000_0111;

        opcode = operationcode{ .opcode4 = opcode4 };

        direction = null;
        displacement_low = null;
        displacement_high = null;
        mode = null;
        reg_mem = null;
        segment_reg = null;

        word = @intCast((bytes[0] & instruct4_word_mask) >> 3);
        reg = @intCast(bytes[0] & instruct4_reg_mask);
        data_low = bytes[1];

        if (word == 0b1) {
            assert(bytes.len >= 3);
            data_high = bytes[2];
            size = 3;
        } else {
            data_high = null;
            size = 2;
        }
    }

    const opcode6: u6 = @intCast((bytes[0] & opcode_mask_6) >> 2);

    if (opcode6 == 0b1000_10) { // register/memory to/from register
        const instruct6_direction_mask: u8 = 0b0000_0010;
        const instruct6_word_mask: u8 = 0b0000_0001;
        const instruct6_mode_mask: u8 = 0b1100_0000;
        const instruct6_reg_mask: u8 = 0b0011_1000;
        const instruct6_reg_mem_mask: u8 = 0b0000_0111;

        opcode = operationcode{ .opcode6 = opcode6 };

        data_low = null;
        data_high = null;
        segment_reg = null;

        direction = @intCast((bytes[0] & instruct6_direction_mask) >> 1);
        word = @intCast(bytes[0] & instruct6_word_mask);
        mode = @intCast((bytes[1] & instruct6_mode_mask) >> 6);
        reg = @intCast((bytes[1] & instruct6_reg_mask) >> 3);
        reg_mem = @intCast(bytes[1] & instruct6_reg_mem_mask);

        if (mode == 0b00 or mode == 0b11) {
            if (mode == 0b00 and reg_mem == 0b110) {
                displacement_low = bytes[2];
                displacement_high = bytes[3];
                size = 4;
            } else {
                displacement_low = null;
                displacement_high = null;
                size = 2;
            }
        } else { // mode == 0b01 or mode == 0b10
            displacement_low = bytes[2];
            displacement_high = if (mode == 0b01) null else bytes[3];
            size = if (mode == 0b01) 3 else 4;
        }
    }

    const opcode7: u7 = @intCast((bytes[0] & opcode_mask_7) >> 1);

    if (opcode7 == 0b1100_011) { // immediate to register/memory
        const instruct7_word_mask = 0b0000_0001;
        const instruct7_mode_mask = 0b1100_0000;
        const instruct7_reg_mem_mask = 0b0000_0111;

        opcode = operationcode{ .opcode7 = opcode7 };

        direction = null;
        reg = null;
        segment_reg = null;

        word = @intCast(bytes[0] & instruct7_word_mask);
        mode = @intCast((bytes[1] & instruct7_mode_mask) >> 6);
        reg_mem = @intCast(bytes[1] & instruct7_reg_mem_mask);

        if (mode == 0b00 or mode == 0b11) {
            if (mode == 0b00 and reg_mem == 0b110) {
                displacement_low = bytes[2];
                displacement_high = bytes[3];
                data_low = bytes[4];
                data_high = if (word == 0b0) null else bytes[5];
                size = if (word == 0b0) 5 else 6;
            } else {
                displacement_low = null;
                displacement_high = null;

                data_low = bytes[2];
                data_high = if (word == 0b0) null else bytes[3];
                size = if (word == 0b0) 3 else 4;
            }
        } else { // mode == 0b01 or mode == 0b10
            displacement_low = bytes[2];
            displacement_high = if (mode == 0b01) null else bytes[3];
            data_low = bytes[4];
            data_high = if (word == 0b0) null else bytes[5];
            size = if (word == 0b0) 5 else 6;
        }
    } else if (opcode7 == 0b1010_000 or opcode7 == 0b1010_001) { // memory/accumlator to memory/accumulator
        const instruct7_word_mask = 0b0000_0001;

        opcode = operationcode{ .opcode7 = opcode7 };

        word = @intCast(bytes[0] & instruct7_word_mask);

        direction = null;
        mode = null;
        reg = null;
        reg_mem = null;
        displacement_high = null;
        displacement_low = null;

        data_low = bytes[1];

        data_high = if (word == 0b0) null else bytes[2];
        size = if (word == 0b0) 2 else 3;
    }

    if (bytes[0] == 0b1000_1110 or bytes[0] == 0b1000_1100) {
        opcode = operationcode{ .opcode8 = bytes[0] };

        const instruct8_mode_mask = 0b1100_0000;
        const instruct8_seg_reg_mask = 0b0001_1000;
        const instruct8_reg_mem_mask = 0b0000_0111;

        direction = null;
        word = null;
        data_low = null;
        data_high = null;
        reg = null;

        mode = @intCast((bytes[1] & instruct8_mode_mask) >> 6);
        segment_reg = @intCast((bytes[1] & instruct8_seg_reg_mask) >> 3);
        reg_mem = @intCast(bytes[1] & instruct8_reg_mem_mask);

        if (mode == 0b00 or mode == 0b11) {
            displacement_low = null;
            size = 2;
        } else { // mode == 0b01 or mode == 0b10
            displacement_low = bytes[2];
            displacement_high = if (mode == 0b01) null else bytes[3];
            size = if (mode == 0b01) 3 else 4;
        }
    }

    return Instruction.init(opcode, direction, word, mode, reg, reg_mem, displacement_low, displacement_high, data_low, data_high, segment_reg, size);
}

pub fn main() !void {
    // Get Allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 0) {
        log.info("USAGE: decoder8086.zig <filepath to assembled file> ...", .{});
        return;
    }

    if (args.len < 2) return error.ExpectedArgument;

    const cwd = std.fs.cwd();

    var buffer: []u8 = try allocator.alloc(u8, 1024);
    defer allocator.free(buffer);

    const line_buffer: []u8 = try allocator.alloc(u8, 32);
    defer allocator.free(line_buffer);

    for (1..args.len) |i| {
        const infile = try cwd.openFile(args[i], .{});
        defer infile.close();

        const bytes_read = try infile.read(buffer);
        var byte_index: usize = 0;

        cwd.makeDir("output") catch |e| switch (e) {
            error.PathAlreadyExists => {},
            else => return e,
        };

        var output_dir = try cwd.openDir("output", .{});
        defer output_dir.close();

        const arg_index: [1]u8 = .{@intCast(i + '0')};

        const outfile = try output_dir.createFile("arg" ++ arg_index ++ ".asm", .{});
        defer outfile.close();

        _ = try outfile.write("16 bits\n\n");

        var instr_len: usize = 2;

        while (byte_index < bytes_read) : (byte_index += instr_len) {
            var instr: Instruction = try CreateInstruction(buffer[byte_index..]);

            instr_len = instr.size;

            const line = try instr.decode(line_buffer);

            _ = try outfile.write(line);
        }
    }
}
