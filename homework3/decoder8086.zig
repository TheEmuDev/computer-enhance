// TODO:(Dean): Implement completionist instructions

const std = @import("std");
const assert = std.debug.assert;
const fmt = std.fmt;
const log = std.log;

const DecoderError = error{ ExpectedArgument, InvalidByteLength, MalformedInstruction, NotImplemented, UnknownOpCode };

const OperationType = enum { opcode4, opcode6, opcode7, opcode8 };
const operationcode = union(OperationType) { opcode4: u4, opcode6: u6, opcode7: u7, opcode8: u8, opcode16: u16 };

const Instruction = struct {
    opcode: operationcode,
    direction: ?u1,
    sign: ?u1,
    word: ?u1,
    rot_cnt: ?u1,
    zero: ?u1,
    mode: ?u2,
    opcode2: ?u3,
    register: ?u3,
    reg_mem: ?u3,
    disp_low: ?u8,
    disp_high: ?u8,
    data_low: ?u8,
    data_high: ?u8,
    segment_reg: ?u2,
    size: usize,

    pub fn init(opcode: operationcode, opcode2: ?u3, d: ?u1, s: ?u1, w: ?u1, v: ?u1, z: ?u1, mod: ?u2, reg: ?u3, reg_mem: ?u3, disp_lo: ?u8, disp_hi: ?u8, data_lo: ?u8, data_hi: ?u8, sr: ?u2, size: usize) Instruction {
        return Instruction{ .opcode = opcode, .opcode2 = opcode2, .direction = d, .sign = s, .rot_cnt = v, .word = w, .zero = z, .mode = mod, .register = reg, .reg_mem = reg_mem, .disp_low = disp_lo, .disp_high = disp_hi, .data_low = data_lo, .data_high = data_hi, .segment_reg = sr, .size = size };
    }

    pub fn decode(self: Instruction, buf: []u8) ![]u8 {
        var data: u16 = undefined;
        var disp: u16 = undefined;
        var msg: []u8 = undefined;
        var operand1: []const u8 = undefined;
        var operand2: []const u8 = undefined;
        var opname: []const u8 = undefined;

        switch (self.opcode) {
            OperationType.opcode8 => |opcode| {
                const data_low = self.data_low orelse return DecoderError.MalformedInstruction;

                switch (opcode) {
                    0b1000_1110, 1000_1100 => {},
                }

                opname = switch (opcode) {
                    0b0111_0100 => "jz",
                    0b0111_1100 => "jnge",
                    0b0111_1110 => "jng",
                    0b0111_0010 => "jnae",
                    0b0111_0110 => "jna",
                    0b0111_1010 => "jpe",
                    0b0111_0000 => "jo",
                    0b0111_1000 => "js",
                    0b0111_0101 => "jnz",
                    0b0111_1101 => "jge",
                    0b0111_1111 => "jg",
                    0b0111_0011 => "jae",
                    0b0111_0111 => "ja",
                    0b0111_1011 => "jpo",
                    0b0111_0001 => "jno",
                    0b0111_1001 => "jns",
                    0b1110_0010 => "loop",
                    0b1110_0001 => "loope",
                    0b1110_0000 => "loopne",
                    0b1110_0011 => "jcxz",
                    else => unreachable
                };

                msg = try fmt.bufPrint(buf, "{s} {d}\n", .{ opname, data_low });
            },
            OperationType.opcode7 => |opcode| {
                switch (opcode) {
                    0b1100_011 => {
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
                                    msg = switch (opcode) {
                                        0b1100_011 => try fmt.bufPrint(buf, "mov [{d}], {d}", .{ disp, data }),
                                        else => unreachable
                                    };
                                } else msg = switch (opcode) {
                                    0b1100_011 => try fmt.bufPrint(buf, "mov {s}, {d}\n", .{ operand1, data }),
                                    else => unreachable
                                };
                            },
                            0b01, 0b10 => {
                                msg = switch (opcode) {
                                    0b1100_011 => try fmt.bufPrint(buf, "mov [{s}{d}], {d}\n", .{ operand1, disp, data }),
                                    else => unreachable
                                };
                            },
                        }
                    },
                    0b0000_010, 0b0001_010, 0b0010_110, 0b0011_110, 0b1010_000 => {
                        const word = self.word orelse return DecoderError.MalformedInstruction;

                        data = getCombined(self.data_low, self.data_high);
                        opname = switch (opcode) {
                            0b0000_010 => "add",
                            0b0001_010 => "adc",
                            0b1010_000 => "mov",
                            0b0010_110 => "sub",
                            0b0011_110 => "cmp",
                            else => unreachable
                        };

                        operand1 = if (word == 0b0) "al" else "ax";

                        msg = try fmt.bufPrint(buf, "{s} {s}, {d}\n", .{ opname, operand1, data });
                    },
                    0b1010_001 => { // MOV: Accumulator to Memory
                        data = getCombined(self.data_low, self.data_high);
                        msg = try fmt.bufPrint(buf, "mov [{d}], ax\n", .{data});
                    },
                    else => {
                        return DecoderError.UnknownOpCode;
                    },
                }
            },
            OperationType.opcode6 => |opcode| {
                switch (opcode) {
                    0b0000_00, 0b0001_00, 0b0010_10, 0b0011_10, 0b1000_10 => {
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

                        opname = switch (opcode) {
                            0b0000_00 => "add",
                            0b0001_00 => "adc",
                            0b0001_10 => "sbb",
                            0b0010_00 => "and",
                            0b0010_10 => "sub",
                            0b0011_10 => "cmp",
                            0b1000_10 => "mov",
                            else => unreachable
                        };

                        operand1 = if (direction == 0b0) decodeRegMem(reg_mem, mode, word) else decodeRegister(register, word);
                        operand2 = if (direction == 0b0) decodeRegister(register, word) else decodeRegMem(reg_mem, mode, word);

                        if ((mode == 0b00 and reg_mem == 0b110) or mode == 0b01 or mode == 0b10) {
                            disp = getCombined(self.disp_low, self.disp_high);
                        }

                        if (direction == 0b0) {
                            if (mode == 0b00 or mode == 0b11) {
                                msg = if (mode == 0b00 and reg_mem == 110) try fmt.bufPrint(buf, "{s} [{d}], {s}\n", .{ opname, disp, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, {s}\n", .{ opname, operand1, operand2 });
                            } else {
                                msg = try fmt.bufPrint(buf, "{s} [{s}{d}], {s}\n", .{ opname, operand1, disp, operand2 });
                            }
                        } else {
                            if (mode == 0b00 or mode == 0b11) {
                                msg = if (mode == 0b00 and reg_mem == 0b110) try fmt.bufPrint(buf, "{s} {s}, [{d}]\n", .{ opname, operand1, disp }) else try fmt.bufPrint(buf, "{s} {s}, {s}\n", .{ opname, operand1, operand2 });
                            } else {
                                msg = try fmt.bufPrint(buf, "{s} {s}, [{s}{d}]\n", .{ opname, operand1, operand2, disp });
                            }
                        }
                    },
                    0b1000_00 => { // immediate to register/memory
                        const sign = self.sign orelse return DecoderError.MalformedInstruction;
                        const word = self.word orelse return DecoderError.MalformedInstruction;
                        const mode = self.mode orelse return DecoderError.MalformedInstruction;
                        const opcode2 = self.opcode2 orelse return DecoderError.MalformedInstruction;
                        const reg_mem = self.reg_mem orelse return DecoderError.MalformedInstruction;
                        const data_low = self.data_low orelse return DecoderError.MalformedInstruction;

                        if ((mode == 0b01 or mode == 0b10) and self.disp_low == null) {
                            return DecoderError.MalformedInstruction;
                        }

                        if (mode == 0b10 and self.disp_high == null) {
                            return DecoderError.MalformedInstruction;
                        }

                        if (sign == 0b0 and word == 0b1 and self.data_high == null) {
                            return DecoderError.MalformedInstruction;
                        }

                        opname = switch (opcode2) {
                            0b000 => "add",
                            0b001 => "or",
                            0b010 => "adc",
                            0b011 => "sbb",
                            0b100 => "and",
                            0b101 => "sub",
                            0b111 => "cmp",
                            else => unreachable,
                        };

                        if ((mode == 0b00 and reg_mem == 0b110) or mode == 0b01 or mode == 0b10) {
                            disp = getCombined(self.disp_low, self.disp_high);
                        }

                        operand1 = decodeRegMem(reg_mem, mode, word);
                        data = getCombined(data_low, self.data_high);

                        if (mode == 0b00 or mode == 0b11) {
                            msg = if (mode == 0b00 and reg_mem == 0b110) try fmt.bufPrint(buf, "{s} [{d}], {d}\n", .{ opname, disp, data }) else try fmt.bufPrint(buf, "{s} {s}, {d}\n", .{ opname, operand1, data });
                        } else {
                            msg = try fmt.bufPrint(buf, "{s} [{s}{d}], {d}\n", .{ opname, operand1, disp, data });
                        }
                    },
                    else => {
                        return DecoderError.UnknownOpCode;
                    },
                }
            },
            OperationType.opcode4 => |opcode| {
                switch (opcode) {
                    0b1011 => {
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
                        return DecoderError.UnknownOpCode;
                    },
                }
            },
        }

        return msg;
    }

    pub fn DebugPrint(self: Instruction) void {
        log.debug("---Instruction-------------", .{});
        log.debug("opcode: {any}", .{self.opcode});
        log.debug("direction: {?b}", .{self.direction});
        log.debug("sign: {?b}", .{self.sign});
        log.debug("word: {?b}", .{self.word});
        log.debug("rot_cnt: {?b}", .{self.rot_cnt});
        log.debug("zero: {?b}", .{self.zero});
        log.debug("mode: {?b}", .{self.mode});
        log.debug("opcode2: {?b}", .{self.opcode2});
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
                return decodeRegister(reg_mem, w);
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

pub fn search(needle: u8, haystack: []u8) bool {
    for (haystack) |candidate| {
        if (candidate == needle) {
            return true;
        }
    }

    return false;
}

pub fn CreateInstruction(bytes: []u8) !Instruction {
    const instr3 = [_]u8{0b000};
    const instr4 = [_]u8{0b1011};
    const instr5 = [_]u8{ 0b0_1010, 0b0_1011, 0b1_0010, 0b0_1000, 0b0_1001, 0b1_1011 };
    const instr6 = [_]u8{ 0b10_0010, 0b00_0000, 0b00_0100, 0b00_1010, 0b00_00110, 0b00_1110, 0b11_0100, 0b00_1000, 0b00_0010, 0b10_0000, 0b00_1100 };
    const instr7 = [_]u8{ 0b111_1001, 0b111_0110, 0b101_0010, 0b101_0011, 0b101_0111, 0b101_0110, 0b101_0101, 0b111_0111, 0b111_0010, 0b111_0011, 0b001_1110, 0b101_0100, 0b101_0000, 0b101_0001, 0b000_0010, 0b000_1010, 0b001_0110, 0b000_1110, 0b001_0010, 0b000_0110, 0b001_1010, 0b100_0011, 0b111_1111, 0b111_1011, 0b110_1010, 0b110_0011, 0b100_0000, 0b001_1010 };
    const instr8 = [_]u8{ 0b1101_0111, 0b1001_1111, 0b1001_1110, 0b1001_1100, 0b1001_1101, 0b0011_0111, 0b0010_0111, 0b0011_1111, 0b0010_1111, 0b1001_1000, 0b1001_1001, 0b1100_0011, 0b1100_1011, 0b1100_1100, 0b1100_1110, 0b1100_1111, 0b1111_1000, 0b1111_0101, 0b1111_1001, 0b1111_1100, 0b1111_1101, 0b1111_1010, 0b1111_1011, 0b1111_0100, 0b1001_1011, 0b1111_0000, 0b1110_1011, 0b0111_0100, 0b0111_1100, 0b0111_1110, 0b0111_0010, 0b0111_0110, 0b0111_1010, 0b0111_0000, 0b0111_1000, 0b0111_0101, 0b0111_1101, 0b0111_1111, 0b0111_0011, 0b0111_0111, 0b0111_1011, 0b0111_0001, 0b0111_1001, 0b1110_0010, 0b1110_0001, 0b1110_0000, 0b1110_0011, 0b1100_1101, 0b1110_1000, 0b1001_1010, 0b1110_1001, 0b1110_1010, 0b1100_0010, 0b1100_1010, 0b1000_1101, 0b1100_0101, 1100_0100, 0b1101_0100, 0b1101_0101, 0b1111_1111, 0b1000_1110, 0b1000_1100 };

    const b76_mask: u8 = 0b1100_0000;
    const b765_mask: u8 = 0b1110_0000;
    const b7654_mask: u8 = 0b1111_0000;
    const b76543_mask: u8 = 0b1111_1000;
    const b765432_mask: u8 = 0b1111_1100;
    const b7654321_mask: u8 = 0b1111_1110;
    const b543_mask: u8 = 0b0011_1000;
    const b43_mask: u8 = 0b0001_1000;
    const b3_mask: u8 = 0b0000_1000;
    const b210_mask: u8 = 0b0000_0111;
    const b1_mask: u8 = 0b0000_0010;
    const b0_mask: u8 = 0b0000_0001;

    var opcode: operationcode = undefined;
    var opcode2: ?u3 = undefined;
    var direction: ?u1 = undefined;
    var sign: ?u1 = undefined;
    var word: ?u1 = undefined;
    var zero: ?u1 = undefined;
    var mode: ?u2 = undefined;
    var reg: ?u3 = undefined;
    var reg_mem: ?u3 = undefined;
    var data_low: ?u8 = undefined;
    var data_high: ?u8 = undefined;
    var displacement_low: ?u8 = undefined;
    var displacement_high: ?u8 = undefined;
    var segment_reg: ?u2 = undefined;
    var size: usize = undefined;

    const opcode3: u8 = @intCast((bytes[0] & b765_mask) >> 5);
    const opcode4: u8 = @intCast((bytes[0] & b7654_mask) >> 4);
    const opcode5: u8 = @intCast((bytes[0] & b76543_mask) >> 3);
    const opcode6: u8 = @intCast((bytes[0] & b765432_mask) >> 2);
    const opcode7: u8 = @intCast((bytes[0] & b7654321_mask) >> 1);

    if (search(bytes[0], instr8) == true) {
        sign = null;
        switch (bytes[0]) {
            0b1101_0111, 0b1001_1111, 0b1001_1110, 0b1001_1100, 0b1001_1101, 0b0011_0111, 0b0010_0111, 0b0011_1111, 0b0010_1111, 0b1001_1000, 0b1001_1001, 0b1100_0011, 0b1100_1011, 0b1100_1100, 0b1100_1110, 0b1100_1111, 0b1111_1000, 0b1111_0101, 0b1111_1001, 0b1111_1100, 0b1111_1101, 0b1111_1010, 0b1111_1011, 0b1111_0100, 0b1001_1011, 0b1111_0000 => {
                opcode = operationcode{ .opcode8 = bytes[0] };
                size = 1;
                opcode2 = null;
                direction = null;
                word = null;
                mode = null;
                reg = null;
                reg_mem = null;
                data_low = null;
                data_high = null;
                displacement_low = null;
                displacement_high = null;
                segment_reg = null;
            },
            0b1110_1011, 0b0111_0100, 0b0111_1100, 0b0111_1110, 0b0111_0010, 0b0111_0110, 0b0111_1010, 0b0111_0000, 0b0111_0101, 0b0111_1101, 0b0111_1111, 0b0111_0011, 0b0111_0111, 0b0111_1011, 0b0111_0001, 0b0111_1001, 0b1110_0010, 0b1110_0001, 0b1110_0000, 0b1110_0011, 0b1100_1101 => {
                opcode = operationcode{ .opcode8 = bytes[0] };
                data_low = bytes[1];
                size = 2;
                opcode2 = null;
                direction = null;
                word = null;
                mode = null;
                reg = null;
                reg_mem = null;
                data_high = null;
                displacement_low = null;
                displacement_high = null;
                segment_reg = null;
            },
            0b1110_1000, 0b1001_1010, 0b1110_1001, 0b1110_1010, 0b1100_0010, 0b1100_1010 => {
                opcode = operationcode{ .opcode8 = bytes[0] };
                data_low = bytes[1];
                data_high = bytes[2];
                size = 3;
                opcode2 = null;
                direction = null;
                word = null;
                mode = null;
                reg = null;
                reg_mem = null;
                displacement_low = null;
                displacement_high = null;
                segment_reg = null;
            },
            0b1111_1111, 0b1000_1111, 0b1000_1110, 0b1000_1100, 0b1000_1101, 0b1100_0101, 0b1100_0100 => {
                opcode = operationcode{ .opcode8 = bytes[0] };
                mode = @intCast((bytes[1] & b76_mask) >> 6);

                reg = if (opcode.opcode8 == 0b1000_1101 or opcode.opcode8 == 0b1100_0101 or opcode.opcode8 == 0b1100_0100) @intCast((bytes[1] & b543_mask) >> 3) else null;
                opcode2 = if (opcode.opcode8 == 0b1111_1111 or opcode.opcode8 == 0b1000_1111) @intCast((bytes[1] & b543_mask) >> 3) else null;
                segment_reg = if (opcode.opcode8 == 0b1000_1110 or opcode.opcode8 == 0b1000_1100) @intCast((bytes[1] & b43_mask) >> 3) else null;
                reg_mem = @intCast((bytes[1] & b210_mask));
                direction = null;
                word = null;

                switch (mode) {
                    0b00, 0b11 => {
                        if (mode == 0b00 and reg_mem == 0b110) {
                            displacement_low = bytes[2];
                            displacement_high = bytes[3];
                            size = 4;
                        } else {
                            displacement_low = null;
                            displacement_high = null;
                            size = 2;
                        }
                    },
                    0b01 => {
                        displacement_low = bytes[2];
                        displacement_high = null;
                        size = 3;
                    },
                    0b10 => {
                        displacement_low = bytes[2];
                        displacement_high = bytes[3];
                        size = 4;
                    },
                }
            },
            0b1101_0100, 0b1101_0101 => {
                if (bytes[1] == 0b0000_1010) {
                    const op16: u16 = if (bytes[0] == 0b1101_0100) 0b1101_0100_0000_1010 else 0b1101_0101_0000_1010;
                    opcode = operationcode{ .opcode16 = op16 };
                    displacement_low = bytes[2];
                    displacement_high = bytes[3];
                    size = 4;
                    opcode2 = null;
                    direction = null;
                    word = null;
                    mode = null;
                    reg = null;
                    reg_mem = null;
                    segment_reg = null;
                }
            },
        }
    } else if (search(opcode7, instr7) == true) {
        // TODO:(Dean): Do I need this enum at all?
        opcode = operationcode{ .opcode7 = @intCast(opcode7) };

        switch (opcode7) {
            0b111_0110, 0b111_0111, 0b111_1001, 0b101_0010, 0b101_0011, 0b101_0111, 0b101_0110, 0b101_0101 => {
                word = if (opcode7 == 0b111_1001) null else @intCast((bytes[0] & b0_mask));
                zero = if (opcode7 == 0b111_1001) @intCast((bytes[0] & b0_mask)) else null;
                size = 1;
                opcode2 = null;
                direction = null;
                sign = null;
                mode = null;
                reg = null;
                reg_mem = null;
                segment_reg = null;
                displacement_low = null;
                displacement_high = null;
                data_low = null;
                data_high = null;
            },
            0b111_0010, 0b111_0011, 0b001_1110, 0b101_0100 => {
                word = @intCast((bytes[0] & b0_mask));
                data_low = bytes[1];
                size = 2;
                zero = null;
                opcode2 = null;
                direction = null;
                sign = null;
                mode = null;
                reg = null;
                reg_mem = null;
                segment_reg = null;
                displacement_low = null;
                displacement_high = null;
                data_high = null;
            },
            0b101_0000, 0b101_0001, 0b000_0010, 0b000_1010, 0b001_0110, 0b000_1110, 0b001_0010, 0b000_0110 => {
                word = @intCast((bytes[0] & b0_mask));
                data_low = bytes[1];
                data_high = if (word == 0b1) bytes[2] else null; // NOTE:(Dean): 0b101_0000 and 0b101_0001 don't mention w as a requirement, might be broken
                size = 3;
                zero = null;
                opcode2 = null;
                direction = null;
                sign = null;
                mode = null;
                reg = null;
                reg_mem = null;
                segment_reg = null;
                displacement_low = null;
                displacement_high = null;
            },
            0b100_0011, 0b111_1111 => {
                word = @intCast((bytes[0] & b0_mask));
                mode = @intCast((bytes[1] & b76_mask) >> 6);
                reg = if (opcode7 == 0b100_0011) @intCast((bytes[1] & b543_mask) >> 3) else null;
                opcode2 = if (opcode7 == 0b100_0011) null else @intCast((bytes[1] & b543_mask) >> 3);
                reg_mem = @intCast((bytes[1] & b210_mask));
                zero = null;
                direction = null;
                sign = null;
                segment_reg = null;
                data_low = null;
                data_high = null;

                switch (mode) {
                    0b00, 0b11 => {
                        if (mode == 0b00 and reg_mem == 0b110) {
                            displacement_low = bytes[2];
                            displacement_high = bytes[3];
                            size = 4;
                        } else {
                            displacement_low = null;
                            displacement_high = null;
                            size = 2;
                        }
                    },
                    0b01 => {
                        displacement_low = bytes[2];
                        displacement_high = null;
                        size = 3;
                    },
                    0b10 => {
                        displacement_low = bytes[2];
                        displacement_high = bytes[3];
                        size = 4;
                    },
                }
            },
            0b110_0011, 0b100_0000, 0b001_1010, 0b111_1011 => {
                word = @intCast((bytes[0] & b0_mask));
                mode = @intCast((bytes[1] & b76_mask) >> 6);
                reg = if (opcode7 == 0b100_0011) @intCast((bytes[1] & b543_mask) >> 3) else null;
                opcode2 = if (opcode7 == 0b100_0011) null else @intCast((bytes[1] & b543_mask) >> 3);
                reg_mem = @intCast((bytes[1] & b210_mask));
                zero = null;

                switch (mode) {
                    0b00, 0b11 => {
                        if (mode == 0b00 and reg_mem == 0b110) {
                            displacement_low = bytes[2];
                            displacement_high = bytes[3];
                            if (opcode7 == 0b111_1011 and opcode2 != 0b000) {
                                data_low = null;
                                data_high = null;
                                size = 4;
                            } else {
                                data_low = bytes[4];
                                data_high = if (word == 0b1) bytes[5] else null;
                                size = if (word == 0b1) 6 else 5;
                            }
                        } else {
                            if (opcode7 == 0b111_1011 and opcode2 != 0b000) {
                                displacement_low = null;
                                displacement_high = null;
                                data_low = null;
                                data_high = null;
                                size = 2;
                            } else {
                                data_low = bytes[2];
                                data_high = if (word == 0b1) bytes[3] else null;
                                size = if (word == 0b1) 4 else 3;
                            }
                        }
                    },
                    0b01 => {
                        displacement_low = bytes[2];
                        displacement_high = null;
                        if (opcode7 == 0b111_1011 and opcode2 != 0b000) {
                            data_low = null;
                            data_high = null;
                            size = 3;
                        } else {
                            data_low = bytes[3];
                            data_high = if (word == 0b1) bytes[4] else null;
                            size = if (word == 0b1) 5 else 4;
                        }
                    },
                    0b10 => {
                        displacement_low = bytes[2];
                        displacement_high = bytes[3];
                        if (opcode7 == 0b111_1011 and opcode2 != 0b000) {
                            data_low = null;
                            data_high = null;
                            size = 4;
                        } else {
                            data_low = bytes[4];
                            data_high = if (word == 0b1) bytes[5] else null;
                            size = if (word == 0b1) 6 else 5;
                        }
                    }
                }
            },
        }
    } else if (search(opcode6, instr6) == true) {
        opcode = operationcode{ .opcode6 = @intCast(opcode6) };
        switch (opcode6) {
            0b00_0000, 0b00_0010, 0b00_0100, 0b00_0110, 0b00_1000, 0b00_1010, 0b00_1100, 0b00_1110, 0b10_0010, 0b11_0100 => {
                direction = @intCast((bytes[0] & b1_mask) >> 1);
                word = @intCast((bytes[0] & b0_mask));
                mode = @intCast((bytes[1] & b76_mask) >> 6);
                reg = if (opcode6 == 0b11_0100) null else @intCast((bytes[1] & b543_mask) >> 3);
                opcode2 = if (opcode6 == 0b11_0100) @intCast((bytes[1] & b543_mask) >> 3) else null;
                reg_mem = @intCast(bytes[1] & b210_mask);
                sign = null;
                segment_reg = null;
                zero = null;

                switch (mode) {
                    0b00, 0b11 => {
                        if (mode == 0b00 and reg_mem == 0b110) {
                            displacement_low = bytes[2];
                            displacement_high = bytes[3];
                            size = 4;
                        } else {
                            displacement_low = null;
                            displacement_high = null;
                            size = 2;
                        }
                    },
                    0b01 => {
                        displacement_low = bytes[2];
                        displacement_high = null;
                        size = 3;
                    },
                    0b10 => {
                        displacement_low = bytes[2];
                        displacement_high = bytes[3];
                        size = 4;
                    }
                }
            },
            0b10_0000 => {
                sign = @intCast((bytes[0] & b1_mask) >> 1);
                word = @intCast((bytes[0] & b0_mask));
                mode = @intCast((bytes[1] & b76_mask) >> 6);
                opcode2 = @intCast((bytes[1] & b543_mask) >> 3);
                reg_mem = @intCast(bytes[1] & b210_mask);
                zero = null;
                direction = null;
                reg = null;
                segment_reg = null;

                switch (mode) {
                    0b00, 0b11 => {
                        if (mode == 0b00 and reg_mem == 0b110) {
                            displacement_low = bytes[2];
                            displacement_high = bytes[3];
                            data_low = bytes[4];
                            data_high = if (sign == 0b0 and word == 0b1) bytes[5] else null;
                            size = if (sign == 0b0 and word == 0b1) 6 else 5;
                        } else {
                            displacement_low = null;
                            displacement_high = null;
                            data_low = bytes[2];
                            data_high = if (sign == 0b0 and word == 0b1) bytes[3] else null;
                            size = if (sign == 0b0 and word == 0b1) 4 else 3;
                        }
                    },
                    0b01 => {
                        displacement_low = bytes[2];
                        displacement_high = null;
                        data_low = bytes[3];
                        data_high = if (sign == 0b0 and word == 0b1) bytes[4] else null;
                        size = if (sign == 0b0 and word == 0b1) 5 else 4;
                    },
                    0b10 => {
                        displacement_low = bytes[2];
                        displacement_high = bytes[3];
                        data_low = bytes[4];
                        data_high = if (sign == 0b0 and word == 0b1) bytes[5] else null;
                        size = if (sign == 0b0 and word == 0b1) 6 else 5;
                    }
                }
            },
        }
    } else if (search(opcode5, instr5) == true) {
        opcode = operationcode{ .opcode5 = @intCast(opcode5) };
        reg = @intCast(bytes[0] & b210_mask);
        size = 1;
        zero = null;
        opcode2 = null;
        direction = null;
        sign = null;
        mode = null;
        reg_mem = null;
        segment_reg = null;
        displacement_low = null;
        displacement_high = null;
        data_low = null;
        data_high = null;
    } else if (search(opcode4, instr4) == true) {
        opcode = operationcode{ .opcode4 = @intCast(opcode4) };
        word = @intCast((bytes[0] & b3_mask) >> 3);
        reg = @intCast(bytes[0] & b210_mask);
        data_low = bytes[1];
        data_high = if (word == 0b1) bytes[2] else null;
        size = if (word == 0b1) 3 else 2;
        opcode2 = null;
        direction = null;
        sign = null;
        mode = null;
        reg_mem = null;
        segment_reg = null;
        displacement_low = null;
        displacement_high = null;
    } else if (search(opcode3, instr3) == true) {
        opcode = operationcode{ .opcode3 = @intCast(opcode3) };
        reg = @intCast((bytes[0] & b43_mask) >> 3);
        opcode2 = @intCast(bytes[0] & b210_mask);
        direction = null;
        sign = null;
        mode = null;
        reg_mem = null;
        segment_reg = null;
        displacement_low = null;
        displacement_high = null;
        data_low = null;
        data_high = null;
    }
    return Instruction.init(opcode, opcode2, direction, sign, word, mode, reg, reg_mem, displacement_low, displacement_high, data_low, data_high, segment_reg, size);
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

        //log.debug("buffer: {b}", .{buffer});

        while (byte_index < bytes_read) : (byte_index += instr_len) {
            var instr: Instruction = try CreateInstruction(buffer[byte_index..]);
            //instr.DebugPrint();

            instr_len = instr.size;

            const line = try instr.decode(line_buffer);

            _ = try outfile.write(line);
        }
    }
}
