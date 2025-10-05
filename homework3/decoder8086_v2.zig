// TODO:(Dean): Implement completionist instructions
// TODO:(Dean): in al, 200 => in al, -56 (instruction on ln 84 in completionist is wrong) (signed vs unsigned issue - low priority)

const std = @import("std");
const assert = std.debug.assert;
const fmt = std.fmt;
const log = std.log;

const DecoderError = error{ ExpectedArgument, InvalidByteLength, MalformedInstruction, NotImplemented, UnknownOpCode };

const Instruction = struct {
    byte_code: []u8,
    source_code: []u8,

    pub fn dump(self: Instruction) void {
        log.debug("Instruction Dump\nbyte code: {b}\nsource code: {s}\n* * * * * * * * * *\n", .{ self.byte_code, self.source_code });
    }
};

var prefix_sr: u3 = undefined;
var use_prefix: bool = false;

fn reset_prefix() void {
    prefix_sr = 0b100;
    use_prefix = false;
}

fn decode_segment_register(sr: u3, set_prefix_flag: bool) []const u8 {
    if (set_prefix_flag) {
        prefix_sr = sr;
        use_prefix = true;
    }

    return switch (sr) {
        0b000 => "es",
        0b001 => "cs",
        0b010 => "ss",
        0b011 => "ds",
        else => unreachable,
    };
}

fn decode_register_memory(reg_mem: u3, mod: u2, w: u1) []const u8 {
    return switch (mod) {
        0b00 => switch (reg_mem) {
            0b000 => "[bx + si]",
            0b001 => "[bx + di]",
            0b010 => "[bp + si]",
            0b011 => "[bp + di]",
            0b100 => "[si]",
            0b101 => "[di]",
            0b110 => "[{d}]", // 16-bit displacement
            0b111 => "[bx]",
        },
        0b01, 0b10 => switch (reg_mem) {
            0b000 => "bx + si",
            0b001 => "bx + di",
            0b010 => "bp + si",
            0b011 => "bp + di",
            0b100 => "si",
            0b101 => "di",
            0b110 => "bp",
            0b111 => "bx",
        },
        0b11 => decode_register(reg_mem, w),
    };
}

fn decode_register(reg: u3, w: u1) []const u8 {
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

fn sign_extend(low: u8) i16 {
    const mask: u16 = if (low & 0x80 == 0x80) 0xFF_00 else 0x00_00;
    return @bitCast(@as(u16, low) | mask);
}

fn get_immediate(low: ?u8, high: ?u8) u16 {
    var combined: u16 = 0x00_00;
    const lo = low orelse 0x00;
    const hi = high orelse 0x00;

    combined = (combined | hi) << 8;
    combined = combined | lo;

    return combined;
}

pub fn create_instruction(bytes: []u8, buf: []u8) !Instruction {
    const b0_mask: u8 = 0b0000_0001;
    const b1_mask: u8 = 0b0000_0010;
    const b210_mask: u8 = 0b0000_0111;
    const b3_mask: u8 = 0b0000_1000;
    const b43_mask: u8 = 0b0001_1000;
    const b543_mask: u8 = 0b0011_1000;
    const b76_mask: u8 = 0b1100_0000;
    const minus = " - ";
    const plus = " + ";
    // const empty = ""; // TODO:(Dean): When displacement is 0, don't print it (ex. [bp + 0] -> [bp] (low priority)

    var data: u16 = undefined;
    var s_data: i16 = undefined;
    var displacement: u16 = undefined;
    var src: []u8 = undefined;
    var prefix: []const u8 = undefined;
    var operand1: []const u8 = undefined;
    var operand2: []const u8 = undefined;
    var opname: []const u8 = undefined;
    var size: usize = undefined;

    var use_signed_displacement: bool = false;

    if (use_prefix) {
        prefix = decode_segment_register(prefix_sr, false);
    }

    switch (bytes[0]) {
        // TODO:(Dean): and [bp - 39], 239 does not create correct src (ln #305 in completionist .asm) low priority (signed vs unsigned)

        // 6-byte | opcode | mod ### r_m | disp-lo | disp-hi | data-lo | data-hi |
        // 5-byte | opcode | mod ### r_m | disp-lo | disp-hi | data-8 |
        // 5-byte | opcode | mod ### r_m | disp-lo | disp-hi | data-sx |
        0x80, 0x81, 0x82, 0x83, 0xC6, 0xC7 => |opcode| {
            const word: u1 = @intCast(opcode & b0_mask);
            const mode: u2 = @intCast((bytes[1] & b76_mask) >> 6);
            const opcode2: u3 = @intCast((bytes[1] & b543_mask) >> 3);
            const r_m: u3 = @intCast(bytes[1] & b210_mask);
            var data_lo: ?u8 = undefined;
            var data_hi: ?u8 = undefined;

            const mem_mode_special_case: bool = (mode == 0b00 and r_m == 0b110);
            const is_0x81_or_0xC7: bool = opcode == 0x81 or opcode == 0xC7;

            // 0x80 - | opcode | mod###r/m | disp-lo | disp-hi | data-8 |
            // 0x81 - | opcode | mod###r/m | disp-lo | disp-hi | data-lo | data-hi |
            // 0x82 - | opcode | mod###r/m | disp-lo | disp-hi | data-8 |
            // 0x83 - | opcode | mod###r/m | disp-lo | disp-hi | data-sx |
            // 0xC6 - | opcode | mod000r/m | disp-lo | disp-hi | data-8 |
            // 0xC7 - | opcode | mod000r/m | disp-lo | disp-hi | data-lo | data-hi |

            switch (mode) {
                0b00, 0b11 => {
                    if (mem_mode_special_case) {
                        displacement = get_immediate(bytes[2], bytes[3]);
                        data_lo = bytes[4];
                        data_hi = if (is_0x81_or_0xC7) bytes[5] else null;
                        size = if (is_0x81_or_0xC7) 6 else 5;
                    } else {
                        data_lo = bytes[2];
                        data_hi = if (is_0x81_or_0xC7) bytes[3] else null;
                        size = if (is_0x81_or_0xC7) 4 else 3;
                    }
                },
                0b01 => {
                    use_signed_displacement = true;
                    displacement = @bitCast(sign_extend(bytes[2]));
                    data_lo = bytes[3];
                    data_hi = if (is_0x81_or_0xC7) bytes[4] else null;
                    size = if (is_0x81_or_0xC7) 5 else 4;
                },
                0b10 => {
                    displacement = get_immediate(bytes[2], bytes[3]);
                    data_lo = bytes[4];
                    data_hi = if (is_0x81_or_0xC7) bytes[5] else null;
                    size = if (is_0x81_or_0xC7) 6 else 5;
                }
            }

            if (opcode == 0xC6 or opcode == 0xC7) {
                opname = if (opcode2 == 0b000) "mov" else unreachable;
            } else {
                opname = switch (opcode2) {
                    0b000 => "add",
                    0b001 => "or",
                    0b010 => "adc",
                    0b011 => "sbb",
                    0b100 => "and",
                    0b101 => "sub",
                    0b110 => "xor",
                    0b111 => "cmp",
                };
            }

            operand1 = decode_register_memory(r_m, mode, word);
            var s_displacement: i16 = @bitCast(displacement);
            var operator = plus;

            if (use_signed_displacement and s_displacement < 0) {
                operator = minus;
                s_displacement = s_displacement * -1;
            }

            s_data = switch (opcode) {
                0x80, 0x82, 0x83, 0xC6 => sign_extend(data_lo.?),
                0x81, 0xC7 => @bitCast(get_immediate(data_lo, data_hi)),
                else => unreachable
            };

            switch (mode) {
                0b00, 0b11 => {
                    if (mem_mode_special_case) {
                        src = try fmt.bufPrint(buf, "{s} [{d}], {d}\n", .{ opname, displacement, s_data });
                    } else {
                        if (use_prefix) {
                            src = try fmt.bufPrint(buf, "{s} {s}:{s}, {d}\n", .{ opname, prefix, operand1, s_data });
                            reset_prefix();
                        } else {
                            src = try fmt.bufPrint(buf, "{s} {s}, {d}\n", .{ opname, operand1, s_data });
                        }
                    }
                },
                0b01 => {
                    if (use_prefix) {
                        src = try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}], {d}\n", .{ opname, prefix, operand1, operator, s_displacement, s_data });
                        reset_prefix();
                    } else {
                        src = try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {d}\n", .{ opname, operand1, operator, s_displacement, s_data });
                    }
                },
                0b10 => {
                    if (use_prefix) {
                        src = try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}], {d}\n", .{ opname, prefix, operand1, operator, displacement, s_data });
                        reset_prefix();
                    } else {
                        src = try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {d}\n", .{ opname, operand1, operator, displacement, s_data });
                    }
                }
            }
        },

        // 5-byte | opcode | ip-lo | ip-hi | cs-lo | cs-hi |
        0x9A, 0xEA => { // TODO:(Dean): Find example of this instruction
            displacement = get_immediate(bytes[1], bytes[2]);
            const cs = get_immediate(bytes[3], bytes[4]);
            size = 5;
            opname = if (bytes[0] == 0x9A) "call" else "jmp";
            src = try fmt.bufPrint(buf, "{s} {d}:{d}\n", .{ opname, cs, displacement });
        },

        // 4-byte | opcode | mod reg r_m | disp-lo | disp-hi |
        // 4-byte | opcode | mod 0sr r_m | disp-lo | disp-hi |
        // 4-byte | opcode | mod ### r_m | disp-lo | disp-hi |
        // NOTE:(Dean): 0xF6 and 0xF7 fit multiple patterns - check opcode2
        0x00, 0x01, 0x02, 0x03, 0x08, 0x09, 0x0A, 0x0B, 0x10, 0x11, 0x12, 0x13, 0x18, 0x19, 0x1A, 0x1B, 0x20, 0x21, 0x22, 0x23, 0x28, 0x29, 0x2A, 0x2B, 0x30, 0x31, 0x32, 0x33, 0x38, 0x39, 0x3A, 0x3B, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, 0xC4, 0xC5, 0xD0, 0xD1, 0xD2, 0xD3, 0xF6, 0xF7, 0xFE, 0xFF => |opcode| {
            // TODO:(Dean): 0x84 - test dh, [bp + 390] is backwards in the output
            const direction: u1 = @intCast((opcode & b1_mask) >> 1);
            const word: u1 = if (opcode == 0xC4) 0b1 else @intCast(opcode & b0_mask);
            const mode: u2 = @intCast((bytes[1] & b76_mask) >> 6);
            const r_op2: u3 = @intCast((bytes[1] & b543_mask) >> 3); // register or opcode 2
            const r_m: u3 = @intCast(bytes[1] & b210_mask);

            const is_0x8C: bool = opcode == 0x8C;
            const is_0xFE: bool = opcode == 0xFE;
            const is_0xF6_or_0xF7_special_case: bool = (opcode == 0xF6 or opcode == 0xF7) and r_op2 == 0b000;
            const mem_mode_special_case: bool = (mode == 0b00 and r_m == 0b110);

            switch (mode) {
                0b00, 0b11 => {
                    if (mem_mode_special_case) {
                        displacement = get_immediate(bytes[2], bytes[3]);
                        if (is_0xF6_or_0xF7_special_case) {
                            data = if (word == 0b1) get_immediate(bytes[4], bytes[5]) else get_immediate(bytes[4], null);
                            size = if (word == 0b1) 6 else 5;
                        } else {
                            size = 4;
                        }
                    } else {
                        displacement = get_immediate(null, null);
                        if (is_0xF6_or_0xF7_special_case) {
                            data = if (word == 0b1) get_immediate(bytes[2], bytes[3]) else get_immediate(bytes[2], null);
                            size = if (word == 0b1) 4 else 3;
                        } else {
                            size = 2;
                        }
                    }
                },
                0b01 => {
                    use_signed_displacement = true;
                    displacement = @bitCast(sign_extend(bytes[2]));
                    size = 3;
                },
                0b10 => {
                    displacement = get_immediate(bytes[2], bytes[3]);
                    size = 4;
                }
            }

            opname = switch (opcode) {
                0x00, 0x01, 0x02, 0x03 => "add",
                0x08, 0x09, 0x0A, 0x0B => "or",
                0x10, 0x11, 0x12, 0x13 => "adc",
                0x18, 0x19, 0x1A, 0x1B => "sbb",
                0x20, 0x21, 0x22, 0x23 => "and",
                0x28, 0x29, 0x2A, 0x2B => "sub",
                0x30, 0x31, 0x32, 0x33 => "xor",
                0x38, 0x39, 0x3A, 0x3B => "cmp",
                0x84, 0x85 => "test",
                0x86, 0x87 => "xchg",
                0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8E => "mov",
                0x8D => "lea",
                0x8F => "pop",
                0xC4 => "les",
                0xC5 => "lds",
                0xD0, 0xD1, 0xD2, 0xD3 => switch (r_op2) {
                    0b000 => "rol",
                    0b001 => "ror",
                    0b010 => "rcl",
                    0b011 => "rcr",
                    0b100 => "sal",
                    0b101 => "shr",
                    0b110 => unreachable,
                    0b111 => "sar",
                }, // opcode2
                0xF6, 0xF7 => switch (r_op2) {
                    0b000 => "test",
                    0b001 => unreachable,
                    0b010 => "not",
                    0b011 => "neg",
                    0b100 => "mul",
                    0b101 => "imul",
                    0b110 => "div",
                    0b111 => "idiv",
                },
                0xFE, 0xFF => switch (r_op2) {
                    0b000 => "inc",
                    0b001 => "dec",
                    0b010 => if (is_0xFE) unreachable else "call",
                    0b011 => if (is_0xFE) unreachable else "call far",
                    0b100 => if (is_0xFE) unreachable else "jmp",
                    0b101 => if (is_0xFE) unreachable else "jmp far",
                    0b110 => if (is_0xFE) unreachable else "push",
                    0b111 => unreachable,
                },
                else => unreachable,
            };

            var s_displacement: i16 = @bitCast(displacement);
            var operator = plus;
            if (use_signed_displacement and s_displacement < 0) {
                operator = minus;
                s_displacement = s_displacement * -1;
            }

            switch (opcode) {
                0x86, 0x87 => {
                    operand1 = decode_register(r_op2, word);
                    operand2 = decode_register_memory(r_m, mode, word);
                    switch (mode) {
                        0b00, 0b11 => {
                            if (mem_mode_special_case) {
                                if (use_prefix) {
                                    src = try fmt.bufPrint(buf, "{s} {s}, {s}:[{d}]\n", .{ opname, operand1, prefix, displacement });
                                    reset_prefix();
                                } else {
                                    src = try fmt.bufPrint(buf, "{s} {s}, [{d}]\n", .{ opname, operand1, displacement });
                                }
                            } else {
                                if (use_prefix) {
                                    src = try fmt.bufPrint(buf, "{s} {s}, {s}:{s}\n", .{ opname, operand1, prefix, operand2 });
                                    reset_prefix();
                                } else {
                                    src = try fmt.bufPrint(buf, "{s} {s}, {s}\n", .{ opname, operand1, operand2 });
                                }
                            }
                        },
                        0b01, 0b10 => {
                            if (use_signed_displacement) {
                                if (use_prefix) {
                                    src = try fmt.bufPrint(buf, "{s} {s}, {s}:[{s}{s}{d}]\n", .{ opname, operand1, prefix, operand2, operator, s_displacement });
                                    reset_prefix();
                                } else {
                                    src = try fmt.bufPrint(buf, "{s} {s}, [{s}{s}{d}]\n", .{ opname, operand1, operand2, operator, s_displacement });
                                }
                            } else {
                                if (use_prefix) {
                                    src = try fmt.bufPrint(buf, "{s} {s}, {s}:[{s}{s}{d}]\n", .{ opname, operand1, prefix, operand2, operator, displacement });
                                    reset_prefix();
                                } else {
                                    src = try fmt.bufPrint(buf, "{s} {s}, [{s}{s}{d}]\n", .{ opname, operand1, operand2, operator, displacement });
                                }
                            }
                        }
                    }
                },
                0x8C, 0x8E => { // TODO:(Dean): use_prefix stuff here?
                    operand1 = if (is_0x8C) decode_register_memory(r_m, mode, word) else decode_segment_register(r_op2, false);
                    operand2 = if (is_0x8C) decode_segment_register(r_op2, false) else decode_register_memory(r_m, mode, word);
                    switch (mode) {
                        0b00, 0b11 => {
                            if (mem_mode_special_case) {
                                src = if (is_0x8C) try fmt.bufPrint(buf, "{s} [{d}], {s}\n", .{ opname, displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, [{d}]\n", .{ opname, operand1, displacement });
                            } else {
                                src = try fmt.bufPrint(buf, "{s} {s}, {s}\n", .{ opname, operand1, operand2 });
                            }
                        },
                        0b01, 0b10 => {
                            if (is_0x8C) {
                                src = if (use_signed_displacement) try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {s}\n", .{ opname, operand1, operator, s_displacement, operand2 }) else try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {s}\n", .{ opname, operand1, operator, displacement, operand2 });
                            } else {
                                src = if (use_signed_displacement) try fmt.bufPrint(buf, "{s} {s}, [{s}{s}{d}]\n", .{ opname, operand1, operand2, operator, s_displacement }) else try fmt.bufPrint(buf, "{s} {s}, [{s}{s}{d}]\n", .{ opname, operand1, operand2, operator, displacement });
                            }
                        }
                    }
                },
                0x8F, 0xD0, 0xD1, 0xD2, 0xD3, 0xF6, 0xF7, 0xFE, 0xFF => {
                    operand1 = decode_register_memory(r_m, mode, word);
                    switch (opcode) {
                        0x8F, 0xF6, 0xF7, 0xFE, 0xFF => {
                            if (is_0xF6_or_0xF7_special_case) {
                                if (mode == 0b00 or mode == 0b11) {
                                    if (mem_mode_special_case) {
                                        if (use_prefix) {
                                            src = try fmt.bufPrint(buf, "{s} {s}:[{d}], {d}\n", .{ opname, prefix, displacement, data });
                                            reset_prefix();
                                        } else {
                                            src = try fmt.bufPrint(buf, "{s} [{d}], {d}\n", .{ opname, displacement, data });
                                        }
                                    } else {
                                        if (use_prefix) {
                                            src = try fmt.bufPrint(buf, "{s} {s}:{s}, {d}\n", .{ opname, prefix, operand1, data });
                                            reset_prefix();
                                        } else {
                                            src = try fmt.bufPrint(buf, "{s} {s}, {d}\n", .{ opname, operand1, data });
                                        }
                                    }
                                } else {
                                    if (use_prefix) {
                                        src = try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}], {d}\n", .{ opname, prefix, operand1, operator, displacement, data });
                                        reset_prefix();
                                    } else {
                                        src = try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {d}\n", .{ opname, operand1, operator, displacement, data });
                                    }
                                }
                            } else {
                                if (mode == 0b00 or mode == 0b11) { // NOTE:(Dean): 0xFF might have some shenanigans here
                                    if (mem_mode_special_case) {
                                        if (use_prefix) {
                                            src = try fmt.bufPrint(buf, "{s} {s}:[{d}]\n", .{ opname, prefix, displacement });
                                            reset_prefix();
                                        } else {
                                            src = try fmt.bufPrint(buf, "{s} [{d}]\n", .{ opname, displacement });
                                        }
                                    } else {
                                        if (use_prefix) {
                                            src = try fmt.bufPrint(buf, "{s} {s}:{s}\n", .{ opname, prefix, operand1 });
                                            reset_prefix();
                                        } else {
                                            src = try fmt.bufPrint(buf, "{s} {s}\n", .{ opname, operand1 });
                                        }
                                    }
                                } else {
                                    if (use_prefix) {
                                        src = if (use_signed_displacement) try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}]\n", .{ opname, prefix, operand1, operator, s_displacement }) else try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}]\n", .{ opname, prefix, operand1, operator, displacement });
                                        reset_prefix();
                                    } else {
                                        src = if (use_signed_displacement) try fmt.bufPrint(buf, "{s} [{s}{s}{d}]\n", .{ opname, operand1, operator, s_displacement }) else try fmt.bufPrint(buf, "{s} [{s}{s}{d}]\n", .{ opname, operand1, operator, displacement });
                                    }
                                }
                            }
                        },
                        0xD0, 0xD1, 0xD2, 0xD3 => {
                            operand2 = if (opcode == 0xD2 or opcode == 0xD3) "cl" else "1";
                            if (mode == 0b00 or mode == 0b11) {
                                if (mem_mode_special_case) {
                                    if (use_prefix) {
                                        src = try fmt.bufPrint(buf, "{s} {s}:[{d}], {s}\n", .{ opname, prefix, displacement, operand2 });
                                        reset_prefix();
                                    } else {
                                        src = try fmt.bufPrint(buf, "{s} [{d}], {s}\n", .{ opname, displacement, operand2 });
                                    }
                                } else {
                                    if (use_prefix) {
                                        src = try fmt.bufPrint(buf, "{s} {s}:{s}, {s}\n", .{ opname, prefix, operand1, operand2 });
                                        reset_prefix();
                                    } else {
                                        src = try fmt.bufPrint(buf, "{s} {s}, {s}\n", .{ opname, operand1, operand2 });
                                    }
                                }
                            } else {
                                if (use_prefix) {
                                    src = if (use_signed_displacement) try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}], {s}\n", .{ opname, prefix, operand1, operator, s_displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}], {s}\n", .{ opname, prefix, operand1, operator, displacement, operand2 });
                                    reset_prefix();
                                } else {
                                    src = if (use_signed_displacement) try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {s}\n", .{ opname, operand1, operator, s_displacement, operand2 }) else try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {s}\n", .{ opname, operand1, operator, displacement, operand2 });
                                }
                            }
                        },
                        else => unreachable
                    }
                },
                // TODO:(Dean): I was here - use_prefix thing
                else => {
                    const is_operand1_reg_mem: bool = direction == 0b0 and opcode != 0x8D and opcode != 0xC4 and opcode != 0xC5;
                    operand1 = if (is_operand1_reg_mem) decode_register_memory(r_m, mode, word) else decode_register(r_op2, word);
                    operand2 = if (is_operand1_reg_mem) decode_register(r_op2, word) else decode_register_memory(r_m, mode, word);
                    if (mode == 0b00 or mode == 0b11) {
                        if (mem_mode_special_case) {
                            src = if (direction == 0b0) try fmt.bufPrint(buf, "{s} [{d}], {s}\n", .{ opname, displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, [{d}]\n", .{ opname, operand1, displacement });
                            if (use_prefix) {
                                src = if (direction == 0b0) try fmt.bufPrint(buf, "{s} {s}:[{d}], {s}\n", .{ opname, prefix, displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, {s}:[{d}]\n", .{ opname, operand1, prefix, displacement });
                                reset_prefix();
                            } else {
                                src = if (direction == 0b0) try fmt.bufPrint(buf, "{s} [{d}], {s}\n", .{ opname, displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, [{d}]\n", .{ opname, operand1, displacement });
                            }
                        } else {
                            if (use_prefix) {
                                src = if (is_operand1_reg_mem) try fmt.bufPrint(buf, "{s} {s}:{s}, {s}\n", .{ opname, prefix, operand1, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, {s}:{s}\n", .{ opname, operand1, prefix, operand2 });
                                reset_prefix();
                            } else {
                                src = try fmt.bufPrint(buf, "{s} {s}, {s}\n", .{ opname, operand1, operand2 });
                            }
                        }
                    } else {
                        if (use_signed_displacement) {
                            if (use_prefix) {
                                src = if (is_operand1_reg_mem) try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}], {s}\n", .{ opname, prefix, operand1, operator, s_displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, {s}:[{s}{s}{d}]\n", .{ opname, operand1, prefix, operand2, operator, s_displacement });
                                reset_prefix();
                            } else {
                                src = if (is_operand1_reg_mem) try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {s}\n", .{ opname, operand1, operator, s_displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, [{s}{s}{d}]\n", .{ opname, operand1, operand2, operator, s_displacement });
                            }
                        } else {
                            if (use_prefix) {
                                src = if (is_operand1_reg_mem) try fmt.bufPrint(buf, "{s} {s}:[{s}{s}{d}], {s}\n", .{ opname, prefix, operand1, operator, displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, {s}:[{s}{s}{d}]\n", .{ opname, operand1, prefix, operand2, operator, displacement });
                                reset_prefix();
                            } else {
                                src = if (is_operand1_reg_mem) try fmt.bufPrint(buf, "{s} [{s}{s}{d}], {s}\n", .{ opname, operand1, operator, displacement, operand2 }) else try fmt.bufPrint(buf, "{s} {s}, [{s}{s}{d}]\n", .{ opname, operand1, operand2, operator, displacement });
                            }
                        }
                    }
                },
            }
        },

        // 4-byte | opcode | mod ### r_m | disp-lo | disp-hi |
        // 4-byte | opcode | mod yyy r_m | disp-lo | disp-hi |
        0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF => |opcode| {
            const esc_op1: u3 = @intCast(opcode & b210_mask);
            const esc_op2: u3 = @intCast((bytes[1] & b543_mask) >> 3);
            const mode: u2 = @intCast((bytes[1] & b76_mask) >> 6);
            const r_m: u3 = @intCast(bytes[1] & b210_mask);

            var esc_opcode: u8 = @as(u8, esc_op1) << 3;
            esc_opcode = esc_opcode | @as(u8, esc_op2);

            opname = "esc";
            operand2 = decode_register_memory(r_m, mode, 0b1);

            switch (mode) {
                0b00, 0b11 => {
                    if (mode == 0b00 and r_m == 0b110) {
                        displacement = get_immediate(bytes[2], bytes[3]);
                        src = try fmt.bufPrint(buf, "{s} {d}, [{d}]\n", .{ opname, esc_opcode, displacement });
                        size = 4;
                    } else {
                        displacement = get_immediate(null, null);
                        src = try fmt.bufPrint(buf, "{s} {d}, {s}\n", .{ opname, esc_opcode, operand2 });
                        size = 2;
                    }
                },
                0b01, 0b10 => {
                    use_signed_displacement = mode == 0b01;
                    displacement = if (use_signed_displacement) @bitCast(sign_extend(bytes[2])) else get_immediate(bytes[2], bytes[3]);
                    var s_displacement: i16 = @bitCast(displacement);
                    var operator = plus;
                    if (use_signed_displacement and s_displacement < 0) {
                        operator = minus;
                        s_displacement = s_displacement * -1;
                    }

                    src = if (use_signed_displacement) try fmt.bufPrint(buf, "{s} {d}, [{s}{s}{d}]\n", .{ opname, esc_opcode, operand2, operator, s_displacement }) else try fmt.bufPrint(buf, "{s} {d}, [{s}{s}{d}]\n", .{ opname, esc_opcode, operand2, operator, displacement });
                    size = if (mode == 0b01) 3 else 4;
                }
            }
        },

        // 3-byte | opcode | data-lo | data-hi |
        // 3-byte | opcode | addr-lo | addr-hi |
        // 3-byte | opcode | ip-inc-lo | ip-inc-hi |
        0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x35, 0x3D, 0xA0, 0xA1, 0xA2, 0xA3, 0xA9, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, 0xC2, 0xCA, 0xE8, 0xE9 => |opcode| {
            const mov_special_case: bool = opcode >= 0xB8 and opcode <= 0xBF;
            const word: u1 = if (mov_special_case) @intCast((opcode & b3_mask) >> 3) else @intCast(opcode & b0_mask);

            data = get_immediate(bytes[1], bytes[2]);
            s_data = @bitCast(data);
            size = 3;

            switch (opcode) {
                0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x35, 0x3D, 0xA9 => {
                    opname = switch (opcode) {
                        0x05 => "add",
                        0x0D => "or",
                        0x15 => "adc",
                        0x1D => "sbb",
                        0x25 => "and",
                        0x2D => "sub",
                        0x35 => "xor",
                        0x3D => "cmp",
                        0xA9 => "test",
                        else => unreachable
                    };
                    operand1 = "ax";
                    data = get_immediate(bytes[1], bytes[2]);
                    src = try fmt.bufPrint(buf, "{s} {s}, {d}\n", .{ opname, operand1, data }); // TODO:(Dean): Can this be combined with the below bucket?
                },
                0xA0, 0xA1, 0xA2, 0xA3 => {
                    const d: u1 = @intCast((opcode & b1_mask) >> 1);
                    opname = "mov";
                    operand1 = if (word == 0b0) "al" else "ax";
                    src = if (d == 0b0) try fmt.bufPrint(buf, "{s} {s}, [{d}]\n", .{ opname, operand1, data }) else try fmt.bufPrint(buf, "{s} [{d}], {s}\n", .{ opname, data, operand1 });
                },
                0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF => {
                    const reg: u3 = @intCast(opcode & b210_mask);
                    opname = "mov";
                    operand1 = decode_register(reg, word);
                    src = try fmt.bufPrint(buf, "{s} {s}, {d}\n", .{ opname, operand1, s_data });
                },
                0xC2, 0xCA => {
                    opname = if (opcode == 0xC2) "ret" else "retf";
                    data = get_immediate(bytes[1], bytes[2]);
                    src = try fmt.bufPrint(buf, "{s} {d}\n", .{ opname, data });
                },
                0xE8, 0xE9 => {
                    opname = if (opcode == 0xE8) "call" else "jmp";
                    src = try fmt.bufPrint(buf, "{s} {d}\n", .{ opname, s_data });
                },
                else => unreachable
            }
        },

        // 2-byte | opcode | data-8 |
        // 2-byte | opcode | ip-inc8 |
        // 2-byte | opcode | opcode |
        0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C, 0x34, 0x3C, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F, 0xA8, 0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xCD, 0xD4, 0xD5, 0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xEB => |opcode| {
            opname = switch (opcode) {
                0x04 => "add",
                0x0C => "or",
                0x14 => "adc",
                0x1C => "sbb",
                0x24 => "and",
                0x2C => "sub",
                0x34 => "xor",
                0x3C => "cmp",
                0x70 => "jo",
                0x71 => "jno",
                0x72 => "jb",
                0x73 => "jnb",
                0x74 => "je",
                0x75 => "jne",
                0x76 => "jbe",
                0x77 => "jnbe",
                0x78 => "js",
                0x79 => "jns",
                0x7A => "jp",
                0x7B => "jnp",
                0x7C => "jl",
                0x7D => "jnl",
                0x7E => "jle",
                0x7F => "jnle",
                0xA8 => "test",
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7 => "mov",
                0xCD => "int",
                0xD4 => if (bytes[1] == 0x0A) "aam" else unreachable,
                0xD5 => if (bytes[1] == 0x0A) "aad" else unreachable,
                0xE0 => "loopne",
                0xE1 => "loope",
                0xE2 => "loop",
                0xE3 => "jcxz",
                0xE4, 0xE5 => "in",
                0xE6, 0xE7 => "out",
                0xEB => "jmp",
                else => unreachable
            };

            s_data = sign_extend(bytes[1]);

            if (opcode < 0x70 or opcode == 0xA8) {
                src = try fmt.bufPrint(buf, "{s} al, {d}\n", .{ opname, s_data });
            } else if (opcode < 0xA8 or opcode == 0xEB or opcode == 0xCD or (opcode >= 0xE0 and opcode <= 0xE3)) {
                src = try fmt.bufPrint(buf, "{s} {d}\n", .{ opname, s_data });
            } else if (opcode < 0xCD) {
                const reg: u3 = @intCast(b210_mask & opcode);
                const word: u1 = 0b0; // NOTE:(Dean): Bit 3 is the word bit: 0xB0 - 0xB7 will always have a 0 in this position
                operand1 = decode_register(reg, word);
                src = try fmt.bufPrint(buf, "{s} {s}, {d}\n", .{ opname, operand1, s_data });
            } else if (opcode < 0xE0) {
                src = try fmt.bufPrint(buf, "{s}\n", .{opname});
            } else if (opcode < 0xEB) {
                const word: u1 = @intCast(opcode & b0_mask);
                src = if (word == 0b0) try fmt.bufPrint(buf, "{s} al, {d}\n", .{ opname, s_data }) else try fmt.bufPrint(buf, "{s} ax, {d}\n", .{ opname, s_data });
            } else {
                unreachable;
            }

            size = 2;
        },

        // 1-byte | opcode |
        0x06, 0x07, 0x0E, 0x16, 0x17, 0x1E, 0x1F, 0x26, 0x27, 0x2E, 0x2F, 0x36, 0x37, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, 0xA4, 0xA5, 0xA6, 0xA7, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, 0xC3, 0xCB, 0xCC, 0xCE, 0xCF, 0xD7, 0xEC, 0xED, 0xEE, 0xEF, 0xF0, 0xF2, 0xF3, 0xF4, 0xF5, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD => |opcode| {
            opname = switch (opcode) {
                0x06, 0x0E, 0x16, 0x1E => "push",
                0x07, 0x17, 0x1F => "pop",
                0x26, 0x2E, 0x36, 0x3E => "",
                0x27 => "daa",
                0x2F => "das",
                0x37 => "aaa",
                0x3F => "aas",
                0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47 => "inc",
                0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F => "dec",
                0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57 => "push",
                0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F => "pop",
                0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97 => "xchg",
                0x98 => "cbw",
                0x99 => "cwd",
                0x9B => "wait",
                0x9C => "pushf",
                0x9D => "popf",
                0x9E => "sahf",
                0x9F => "lahf",
                0xA4, 0xA5 => "movs",
                0xA6, 0xA7 => "cmps",
                0xAA, 0xAB => "stos",
                0xAC, 0xAD => "lods",
                0xAE, 0xAF => "scas",
                0xC3 => "ret",
                0xCB => "retf",
                0xCC => "int",
                0xCE => "into",
                0xCF => "iret",
                0xD7 => "xlat",
                0xEC, 0xED => "in",
                0xEE, 0xEF => "out",
                0xF0 => "lock",
                0xF2 => "repne",
                0xF3 => "rep",
                0xF4 => "hlt",
                0xF5 => "cmc",
                0xF8 => "clc",
                0xF9 => "stc",
                0xFA => "cli",
                0xFB => "sti",
                0xFC => "cld",
                0xFD => "std",
                else => unreachable,
            };

            size = 1;

            if (opcode <= 0x1F) {
                const sr: u3 = @intCast((opcode & b43_mask) >> 3);
                operand1 = decode_segment_register(sr, false);
                src = try fmt.bufPrint(buf, "{s} {s}\n", .{ opname, operand1 });
            } else if (opcode <= 0x3F) {
                const sr: u3 = @intCast((opcode & b43_mask) >> 3);
                operand1 = if (opcode == 0x26 or opcode == 0x2E or opcode == 0x36 or opcode == 0x3E) decode_segment_register(sr, true) else ""; //decode_segment_register(sr, false);
                switch (opcode) {
                    0x26, 0x2E, 0x36, 0x3E => {
                        src = "";
                    },
                    0x27, 0x2F, 0x37, 0x3F => src = try fmt.bufPrint(buf, "{s}\n", .{opname}),
                    else => unreachable,
                }
            } else if (opcode <= 0x97) {
                const reg: u3 = @intCast(opcode & b210_mask);
                operand1 = decode_register(reg, 0b1);
                src = if (opcode >= 0x90) try fmt.bufPrint(buf, "{s} ax, {s}\n", .{ opname, operand1 }) else try fmt.bufPrint(buf, "{s} {s}\n", .{ opname, operand1 });
            } else if (opcode >= 0xA4 and opcode <= 0xCB) {
                if (opcode < 0xC3) {
                    const w_z: u1 = @intCast(opcode & b0_mask);
                    src = if (w_z == 0b0) try fmt.bufPrint(buf, "{s}b\n", .{opname}) else try fmt.bufPrint(buf, "{s}w\n", .{opname});
                } else {
                    src = try fmt.bufPrint(buf, "{s}\n", .{opname});
                }
            } else if (opcode == 0xCC) {
                src = try fmt.bufPrint(buf, "{s} 3\n", .{opname});
            } else if (opcode <= 0xD7) {
                src = try fmt.bufPrint(buf, "{s}\n", .{opname});
            } else if (opcode <= 0xEF) {
                const w: u1 = @intCast(opcode & b0_mask);
                src = if (w == 0b0) try fmt.bufPrint(buf, "{s} al, dx\n", .{opname}) else try fmt.bufPrint(buf, "{s} ax, dx\n", .{opname});
            } else {
                src = if (opcode <= 0xF3) try fmt.bufPrint(buf, "{s} ", .{opname}) else try fmt.bufPrint(buf, "{s}\n", .{opname});
            }
        },

        // unused
        0x0F, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0xC0, 0xC1, 0xC8, 0xC9, 0xD6, 0xF1 => unreachable,
    }
    return Instruction{ .byte_code = bytes[0..size], .source_code = src };
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

        // var instr_len: usize = 2;
        var instr_len: usize = undefined;

        log.debug("buffer: {b}", .{buffer});

        while (byte_index < bytes_read) : (byte_index += instr_len) {
            const instr: Instruction = try create_instruction(buffer[byte_index..], line_buffer);
            instr_len = instr.byte_code.len;
            instr.dump(); // NOTE::DEBUG
            _ = try outfile.write(instr.source_code);
        }
    }
}
