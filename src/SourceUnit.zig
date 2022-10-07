const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const utils = @import("utils.zig");
const Package = @import("Package.zig");
const tv = @import("types_and_values.zig");
const LiveScope = @import("LiveScope.zig");

const SourceUnit = @This();

allocator: std.mem.Allocator,
package: *Package,
path: []const u8,
source: [:0]const u8,
tree: zig.Ast,

type_info: std.ArrayListUnmanaged(tv.TypeInfo) = .{},
type_info_map: std.HashMapUnmanaged(tv.TypeInfo, usize, TypeInfoContext, std.hash_map.default_max_load_percentage) = .{},

root_scope: *LiveScope = undefined,

// Scopes based on AST location
// static_scopes: std.ArrayListUnmanaged(Scope) = .{},

pub const TypeInfoContext = struct {
    unit: SourceUnit,
    hasher: *std.hash.Wyhash,

    pub fn hash(self: @This(), s: tv.TypeInfo) u64 {
        tv.TypeInfo.hash(self, s);
        return self.hasher.final();
    }
    pub fn eql(self: @This(), a: tv.TypeInfo, b: tv.TypeInfo) bool {
        return tv.TypeInfo.eql(self.unit, a, b);
    }
};

pub const TypeFormatter = struct {
    unit: *SourceUnit,
    @"type": tv.Type,

    pub fn format(value: TypeFormatter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{a]ny} @ node {d}", .{ value.unit.formatTypeInfo(value.unit.type_info.items[value.@"type".info_idx]), value.@"type".node_idx });
    }
};

pub fn formatType(unit: *SourceUnit, @"type": tv.Type) TypeFormatter {
    return TypeFormatter{ .unit = unit, .@"type" = @"type" };
}

pub const TypeInfoFormatter = struct {
    unit: *SourceUnit,
    ti: tv.TypeInfo,

    pub fn format(value: TypeInfoFormatter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        return switch (value.ti) {
            .int => |ii| switch (ii.signedness) {
                .signed => try writer.print("i{d}", .{ii.bits}),
                .unsigned => try writer.print("u{d}", .{ii.bits}),
            }, // TODO
            .float => |f| try writer.print("f{d}", .{f}),
            .@"comptime_int" => try writer.writeAll("comptime_int"),
            .@"comptime_float" => try writer.writeAll("comptime_float"),
            .@"type" => try writer.writeAll("type"),
            .@"bool" => try writer.writeAll("bool"),
            .@"struct" => |s| {
                try writer.writeAll("struct {");
                var iterator = s.scope.declarations.iterator();
                while (iterator.next()) |di| {
                    const decl = di.value_ptr.*;
                    if (decl.isConstant(value.unit.tree)) {
                        try writer.print("const {s}: , {any}", .{ decl.name, value.unit.formatTypeInfo(value.unit.type_info.items[decl.@"type".info_idx]) });
                    } else {
                        try writer.print("var {s}: , {any}", .{ decl.name, value.unit.formatTypeInfo(value.unit.type_info.items[decl.@"type".info_idx]) });
                    }
                }
                try writer.writeAll("}");
            },
            else => try writer.print("UnimplementedTypeInfoPrint", .{}),
        };
    }
};

pub fn formatTypeInfo(unit: *SourceUnit, ti: tv.TypeInfo) TypeInfoFormatter {
    return TypeInfoFormatter{ .unit = unit, .ti = ti };
}

// pub const Value =

pub const InitError = std.mem.Allocator.Error || std.fs.File.OpenError || std.fs.File.ReadError;

/// Caller owns all inputs
pub fn init(allocator: std.mem.Allocator, package: *Package, path: []const u8) InitError!SourceUnit {
    var file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    const source = try file.readToEndAllocOptions(
        allocator,
        std.math.maxInt(usize),
        null,
        @alignOf(u8),
        0,
    );
    errdefer allocator.free(source);

    var tree = try std.zig.parse(allocator, source);
    errdefer tree.deinit(allocator);

    return SourceUnit{
        .allocator = allocator,
        .package = package,
        .path = path,
        .source = source,
        .tree = tree,
    };
}

pub fn deinit(unit: *SourceUnit) void {
    unit.tree.deinit(unit.allocator);
    unit.allocator.free(unit.source);
}

/// Creates type, returns type
pub fn createType(unit: *SourceUnit, node_idx: Ast.Node.Index, type_info: tv.TypeInfo) std.mem.Allocator.Error!tv.Type {
    // TODO: Figure out dedup
    var hasher = std.hash.Wyhash.init(0);
    var gpr = try unit.type_info_map.getOrPutContext(unit.allocator, type_info, .{ .unit = unit.*, .hasher = &hasher });

    if (gpr.found_existing) {
        std.log.info("Deduplicating type {d}", .{unit.formatTypeInfo(unit.type_info.items[gpr.value_ptr.*])});
        return tv.Type{ .node_idx = node_idx, .info_idx = gpr.value_ptr.* };
    } else {
        try unit.type_info.append(unit.allocator, type_info);
        const info_idx = unit.type_info.items.len - 1;
        gpr.value_ptr.* = info_idx;
        return tv.Type{ .node_idx = node_idx, .info_idx = info_idx };
    }
}

pub fn typeFromTypeValue(unit: *SourceUnit, value: tv.Value) tv.Type {
    std.debug.assert(unit.type_info.items[value.@"type".info_idx] == .@"type");
    // return @ptrCast(*tv.Type, @alignCast(@alignOf(*tv.Type), value.value)).*;
}

pub const InterpretResult = union(enum) {
    @"break": ?[]const u8,
    break_with_value: struct {
        label: ?[]const u8,
        value: tv.Value,
    },
    value: tv.Value,
    scope: *LiveScope,
    nothing,

    pub fn maybeGetValue(result: InterpretResult) ?tv.Value {
        return switch (result) {
            .break_with_value => |v| v.value,
            .value => |v| v,
            else => null,
        };
    }

    pub fn getValue(result: InterpretResult) tv.Value {
        return result.maybeGetValue() orelse @panic("Attempted to get value from non-value interpret result");
    }
};

pub fn addDeclaration(unit: *SourceUnit, scope_idx: usize, declaration: tv.Declaration) std.mem.Allocator.Error!void {
    try unit.declarations.append(unit.allocator, declaration);
    try unit.declaration_map.put(unit.allocator, .{ .scope_idx = scope_idx, .name = utils.getDeclName(unit.tree, declaration.node_idx).? }, unit.declarations.items.len - 1);
}

pub fn newScope(unit: *SourceUnit, parent: ?*LiveScope, node_idx: Ast.Node.Index) !*LiveScope {
    var ls = try unit.allocator.create(LiveScope);
    ls.* = .{
        .allocator = unit.allocator,
        .parent = parent,
        .node_idx = node_idx,
    };
    return ls;
}

pub const InterpretOptions = struct {
    /// Whether or not this interpretation cares about values;
    /// this is always true in type determination / comptime
    observe_values: bool,
    /// Whether or not an interpretation zone is comptime
    is_comptime: bool,
};

pub const InterpretError = std.mem.Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError || error{ InvalidCharacter, InvalidBase };
pub fn interpret(
    unit: *SourceUnit,
    node_idx: Ast.Node.Index,
    live_scope: ?*LiveScope,
    options: InterpretOptions,
) InterpretError!InterpretResult {
    // _ = unit;
    // _ = node;
    // _ = observe_values;

    const tree = unit.tree;
    const tags = tree.nodes.items(.tag);
    const data = tree.nodes.items(.data);
    const main_tokens = tree.nodes.items(.main_token);

    std.log.info("{any}", .{tags[node_idx]});

    switch (tags[node_idx]) {
        .container_decl,
        .container_decl_trailing,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .root,
        .error_set_decl,
        => {
            var container_scope = try unit.newScope(live_scope, node_idx);
            var type_info = tv.TypeInfo{
                .@"struct" = .{
                    .scope = container_scope,
                },
            };

            if (node_idx == 0) unit.root_scope = container_scope;

            var buffer: [2]Ast.Node.Index = undefined;
            const members = utils.declMembers(tree, node_idx, &buffer);

            for (members) |member| {
                const maybe_container_field: ?zig.Ast.full.ContainerField = switch (tags[member]) {
                    .container_field => tree.containerField(member),
                    .container_field_align => tree.containerFieldAlign(member),
                    .container_field_init => tree.containerFieldInit(member),
                    else => null,
                };

                if (maybe_container_field) |field_info| {
                    var init_type = try unit.interpret(field_info.ast.type_expr, live_scope, .{ .observe_values = true, .is_comptime = true });
                    var default_value = if (field_info.ast.value_expr == 0)
                        null
                    else
                        (try unit.interpret(field_info.ast.value_expr, live_scope, .{ .observe_values = true, .is_comptime = true })).getValue();

                    const name = tree.tokenSlice(field_info.ast.name_token);
                    const field = tv.FieldDefinition{
                        .node_idx = member,
                        .name = name,
                        .@"type" = init_type.getValue().value_data.@"type",
                        .default_value = default_value,
                        // TODO: Default values
                        // .@"type" = T: {
                        //     var value = (try unit.interpret(field_info.ast.type_expr, scope_idx, true)).?.value;
                        //     break :T @ptrCast(*tv.Type, @alignCast(@alignOf(*tv.Type), value)).*;
                        // },
                        // .value = null,
                    };

                    try type_info.@"struct".fields.put(unit.allocator, name, field);
                } else {
                    _ = try unit.interpret(member, live_scope, options);
                }
            }

            return InterpretResult{ .value = tv.Value{
                .node_idx = node_idx,
                .@"type" = try unit.createType(node_idx, .{ .@"type" = .{} }),
                .value_data = .{ .@"type" = try unit.createType(node_idx, type_info) },
            } };
        },
        .global_var_decl,
        .local_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        => {
            const decl = utils.varDecl(tree, node_idx).?;
            var value = (try unit.interpret(decl.ast.init_node, live_scope, options)).getValue();
            var @"type" = if (decl.ast.type_node == 0) tv.Value{
                .node_idx = std.math.maxInt(Ast.Node.Index),
                .@"type" = try unit.createType(node_idx, .{ .@"type" = .{} }),
                .value_data = .{ .@"type" = value.@"type" },
            } else (try unit.interpret(decl.ast.type_node, live_scope, options)).getValue();

            const name = utils.getDeclName(tree, node_idx).?;
            try live_scope.?.declarations.put(unit.allocator, name, .{
                .node_idx = node_idx,
                .name = name,
                .@"type" = @"type".value_data.@"type",
                .@"value" = value,
            });

            return InterpretResult{ .nothing = .{} };
        },
        // .block,
        // .block_semicolon,
        // .block_two,
        // .block_two_semicolon,
        // => {
        //     // try unit.scopes.append(unit.allocator, .{
        //     //     .node_idx = node_idx,
        //     //     .parent_scope = parent_scope_idx orelse std.math.maxInt(usize),
        //     // });
        //     // const scope_idx = unit.scopes.items.len - 1;

        //     var scope = try unit.newScope(live_scope, node_idx);

        //     var buffer: [2]Ast.Node.Index = undefined;
        //     const statements = utils.blockStatements(tree, node_idx, &buffer).?;

        //     for (statements) |idx| {
        //         const ret = try unit.interpret(idx, scope, options);
        //         switch (ret) {
        //             .@"break" => |lllll| {
        //                 const maybe_block_label_string = if (unit.scopes.items[scope_idx].getLabel(tree)) |i| tree.tokenSlice(i) else null;
        //                 if (lllll) |l| {
        //                     if (maybe_block_label_string) |ls| {
        //                         if (std.mem.eql(u8, l, ls)) {
        //                             return InterpretResult{ .nothing = .{} };
        //                         } else return ret;
        //                     } else return ret;
        //                 } else {
        //                     return InterpretResult{ .nothing = .{} };
        //                 }
        //             },
        //             .break_with_value => |bwv| {
        //                 const maybe_block_label_string = if (unit.scopes.items[scope_idx].getLabel(tree)) |i| tree.tokenSlice(i) else null;

        //                 if (bwv.label) |l| {
        //                     if (maybe_block_label_string) |ls| {
        //                         if (std.mem.eql(u8, l, ls)) {
        //                             return InterpretResult{ .value = bwv.value };
        //                         } else return ret;
        //                     } else return ret;
        //                 } else {
        //                     return InterpretResult{ .value = bwv.value };
        //                 }
        //             },
        //             else => {},
        //         }
        //     }

        //     return InterpretResult{ .nothing = .{} };
        // },
        .identifier => {
            var value = tree.getNodeSource(node_idx);

            if (std.mem.eql(u8, "type", value)) {
                return InterpretResult{ .value = tv.Value{
                    .node_idx = node_idx,
                    .@"type" = try unit.createType(node_idx, .{ .@"type" = .{} }),
                    .value_data = .{ .@"type" = try unit.createType(node_idx, .{ .@"type" = .{} }) },
                } };
            } else if (value.len >= 2 and (value[0] == 'u' or value[0] == 'i')) int: {
                return InterpretResult{ .value = tv.Value{
                    .node_idx = node_idx,
                    .@"type" = try unit.createType(node_idx, .{ .@"type" = .{} }),
                    .value_data = .{ .@"type" = try unit.createType(node_idx, .{
                        .int = .{
                            .signedness = if (value[0] == 'u') .unsigned else .signed,
                            .bits = std.fmt.parseInt(u16, value[1..], 10) catch break :int,
                        },
                    }) },
                } };
            }

            // Logic to find identifiers in accessible scopes

            var psi = live_scope.?.parentScopeIterator();
            while (psi.next()) |scope| {
                return InterpretResult{ .value = (scope.declarations.get(value) orelse continue).value };
            }

            std.log.err("Identifier not found: {s}", .{value});
            @panic("Could not find identifier");
        },
        .grouped_expression => {
            return try unit.interpret(data[node_idx].lhs, live_scope, options);
        },
        .@"break" => {
            const label = if (data[node_idx].lhs == 0) null else tree.tokenSlice(data[node_idx].lhs);
            return if (data[node_idx].rhs == 0)
                InterpretResult{ .@"break" = label }
            else
                InterpretResult{ .break_with_value = .{ .label = label, .value = (try unit.interpret(data[node_idx].rhs, live_scope, options)).getValue() } };
        },
        .@"if", .if_simple => {
            const iff = utils.ifFull(tree, node_idx);
            if (options.observe_values) {
                const ir = try unit.interpret(iff.ast.cond_expr, live_scope, options);
                if (ir.getValue().value_data.@"bool") {
                    return try unit.interpret(iff.ast.then_expr, live_scope, options);
                } else {
                    if (iff.ast.else_expr != 0) {
                        return try unit.interpret(iff.ast.else_expr, live_scope, options);
                    } else return InterpretResult{ .nothing = .{} };
                }
            } else {
                _ = try unit.interpret(iff.ast.cond_expr, live_scope, options);
                _ = try unit.interpret(iff.ast.then_expr, live_scope, options);
                _ = try unit.interpret(iff.ast.else_expr, live_scope, options);
            }
            @panic("bruh");
        },
        .equal_equal => {
            var a = try unit.interpret(data[node_idx].lhs, live_scope, options);
            var b = try unit.interpret(data[node_idx].rhs, live_scope, options);
            return InterpretResult{ .value = tv.Value{
                .node_idx = node_idx,
                .@"type" = try unit.createType(node_idx, .{ .@"bool" = .{} }),
                .value_data = .{ .@"bool" = a.getValue().eql(b.getValue()) },
            } };
            // a.getValue().eql(b.getValue())
        },
        .number_literal => {
            const s = tree.getNodeSource(node_idx);
            const nl = std.zig.parseNumberLiteral(s);
            // if (nl == .failure) ;
            return InterpretResult{ .value = tv.Value{
                .node_idx = node_idx,
                .@"type" = try unit.createType(node_idx, .{ .@"comptime_int" = .{} }),
                .value_data = switch (nl) {
                    .float => .{ .float = try std.fmt.parseFloat(f64, s) },
                    .int => if (s[0] == '-') tv.ValueData{ .signed_int = try std.fmt.parseInt(i64, s, 0) } else tv.ValueData{ .unsigned_int = try std.fmt.parseInt(u64, s, 0) },
                    .big_int => |bii| ppp: {
                        var bi = try std.math.big.int.Managed.init(unit.allocator);
                        try bi.setString(@enumToInt(bii), s[if (bii != .decimal) @as(usize, 2) else @as(usize, 0)..]);
                        break :ppp .{ .@"comptime_int" = bi };
                    },
                    .failure => @panic("Failed to parse number literal"),
                },
            } };
        },
        .assign,
        .assign_bit_and,
        .assign_bit_or,
        .assign_shl,
        .assign_shr,
        .assign_bit_xor,
        .assign_div,
        .assign_sub,
        .assign_sub_wrap,
        .assign_mod,
        .assign_add,
        .assign_add_wrap,
        .assign_mul,
        .assign_mul_wrap,
        => {
            // TODO: Make this work with non identifiers
            // TODO: Actually consider operators

            if (options.observe_values) {
                const value = tree.getNodeSource(data[node_idx].lhs);

                var psi = live_scope.?.parentScopeIterator();
                while (psi.next()) |scope| {
                    if (scope.declarations.getEntry(value)) |decl|
                        decl.value_ptr.value = (try unit.interpret(data[node_idx].rhs, live_scope.?, options)).getValue();
                }

                return InterpretResult{ .nothing = .{} };
            } else {
                // TODO: ...
                // const value = tree.getNodeSource(data[node_idx].lhs);

                // var psi = unit.parentScopeIterator(live_scope.?);
                // while (psi.next()) |i| {
                //     if (unit.declaration_map.get(.{ .scope_idx = i, .name = value })) |_| {
                //         _ = try unit.interpret(data[node_idx].lhs, i, options);
                //         _ = try unit.interpret(data[node_idx].rhs, i, options);
                //         // TODO: Register declaration use site
                //     }
                // }

                return InterpretResult{ .nothing = .{} };
            }
        },
        // .@"switch",
        // .switch_comma,
        // => {
        //     const cond = data[node_idx].lhs;
        //     const extra = tree.extraData(data[node_idx].rhs, Ast.Node.SubRange);
        //     const cases = tree.extra_data[extra.start..extra.end];

        //     for (cases) |case| {
        //         const switch_case: Ast.full.SwitchCase = switch (tags[case]) {
        //             .switch_case => tree.switchCase(case),
        //             .switch_case_one => tree.switchCaseOne(case),
        //             else => continue,
        //         };
        //     }
        // },
        .builtin_call,
        .builtin_call_comma,
        .builtin_call_two,
        .builtin_call_two_comma,
        => {
            var buffer: [2]Ast.Node.Index = undefined;
            const params = utils.builtinCallParams(tree, node_idx, &buffer).?;
            const call_name = tree.tokenSlice(main_tokens[node_idx]);

            if (std.mem.eql(u8, call_name, "@compileLog")) {
                pp: for (params) |param| {
                    const res = (try unit.interpret(param, live_scope, .{ .observe_values = true, .is_comptime = true })).getValue();
                    const ti = unit.type_info.items[res.@"type".info_idx];
                    switch (ti) {
                        .pointer => |ptr| {
                            const child = unit.type_info.items[ptr.child.info_idx];
                            if (ptr.size == .slice and child == .int and child.int.bits == 8 and child.int.signedness == .unsigned) {

                                // TODO: Fix once I optimize slices
                                std.debug.print("@compileLog output: ", .{});
                                for (res.value_data.slice_ptr.items) |i| std.debug.print("{c}", .{@truncate(u8, i.unsigned_int)});
                                std.debug.print("\n", .{});

                                break :pp;
                            }
                        },
                        else => {},
                    }

                    @panic("compileLog argument type not printable!");
                }

                return InterpretResult{ .nothing = .{} };
            }

            std.log.info("Builtin not implemented: {s}", .{call_name});
            @panic("Builtin not implemented");
        },
        .string_literal => {
            const value = tree.getNodeSource(node_idx)[1 .. tree.getNodeSource(node_idx).len - 1];
            var val = tv.Value{
                .node_idx = node_idx,
                .@"type" = try unit.createType(node_idx, .{
                    .pointer = .{
                        .size = .slice,
                        .is_const = true,
                        .is_volatile = false,
                        .child = try unit.createType(0, .{ .int = .{
                            .bits = 8,
                            .signedness = .unsigned,
                        } }),
                        .is_allowzero = false,

                        .sentinel = .{ .unsigned_int = 0 },
                    },
                }),
                .value_data = .{ .slice_ptr = .{} },
            };

            for (value) |z| {
                try val.value_data.slice_ptr.append(unit.allocator, .{ .unsigned_int = z });
            }
            try val.value_data.slice_ptr.append(unit.allocator, .{ .unsigned_int = 0 });

            return InterpretResult{ .value = val };
        },
        .@"comptime" => {
            return try unit.interpret(data[node_idx].lhs, live_scope, .{ .observe_values = true, .is_comptime = true });
        },
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => {
            // var buf: [1]Ast.Node.Index = undefined;
            // const func = utils.fnProto(tree, node_idx, &buf).?;

            // try unit.scopes.append(unit.allocator, .{
            //     .node_idx = node_idx,
            //     .parent_scope = parent_scope_idx orelse std.math.maxInt(usize),
            // });
            // const func_scope_idx = unit.scopes.items.len - 1;

            // var fnd: tv.TypeInfo.Fn = .{
            //     .return_type = null,
            // };

            // var it = func.iterate(&tree);
            // while (utils.nextFnParam(&it)) |param| {
            //     // Add parameter decls
            //     if (param.name_token) |name_token| {
            //         // TODO: Think of new method for functions
            //         if ((try unit.interpret(param.type_expr, func_scope_idx, .{ .observe_values = true, .is_comptime = true })).maybeGetValue()) |value| {
            //             try unit.addDeclaration(func_scope_idx, value.value_data.@"type");
            //             try fnd.params.append(unit.allocator, unit.declarations.items.len - 1);
            //         } else {
            //             try unit.addDeclaration(parent_scope_idx.?, .{
            //                 .node_idx = node_idx,
            //                 .name = tree.tokenSlice(name_token),
            //                 .scope_idx = func_scope_idx, // orelse std.math.maxInt(usize),
            //                 .@"value" = undefined,
            //                 .@"type" = unit.createType(0, .{ .@"anytype" = .{} }),
            //             });
            //             try fnd.params.append(unit.allocator, unit.declarations.items.len - 1);
            //         }
            //     }
            // }

            // if ((try unit.interpret(func.ast.return_type, func_scope_idx, .{ .observe_values = true, .is_comptime = true })).maybeGetValue()) |value|
            //     fnd.return_type = value.value_data.@"type";

            // if (func.fn_tag == .fn_decl) {
            //     //a
            // } else {
            //     return InterpretResult{};
            // }
            return InterpretResult{ .nothing = .{} };
        },
        else => {
            std.log.err("Unhandled {any}", .{tags[node_idx]});
            return InterpretResult{ .nothing = .{} };
        },
    }
}
