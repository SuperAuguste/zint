const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const utils = @import("utils.zig");
const Package = @import("Package.zig");
const tv = @import("types_and_values.zig");

const SourceUnit = @This();

allocator: std.mem.Allocator,
package: *Package,
path: []const u8,
source: [:0]const u8,
tree: zig.Ast,

type_info: std.ArrayListUnmanaged(tv.TypeInfo) = .{},
type_info_map: std.HashMapUnmanaged(tv.TypeInfo, usize, TypeInfoContext, std.hash_map.default_max_load_percentage) = .{},

scopes: std.ArrayListUnmanaged(Scope) = .{},
fields: std.ArrayListUnmanaged(tv.Field) = .{},
declarations: std.ArrayListUnmanaged(tv.Declaration) = .{},

declaration_map: std.HashMapUnmanaged(DeclarationLookup, usize, DeclarationMapContext, std.hash_map.default_max_load_percentage) = .{},

// declaration_use_sites: std.ArrayListUnmanaged(DeclarationUseSite) = .{},
// field_use_sites: std.ArrayListUnmanaged(FieldUseSite) = .{},

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

pub const DeclarationLookup = struct { scope_idx: usize, name: []const u8 };
pub const DeclarationMapContext = struct {
    pub fn hash(self: @This(), s: DeclarationLookup) u64 {
        _ = self;
        // TODO: Make this not terribly extremely dangerous
        var scope_idx_buf = std.mem.toBytes(s.scope_idx);

        var hasher = std.hash.Wyhash.init(0);
        hasher.update(s.name);
        hasher.update(&scope_idx_buf);
        return hasher.final();
    }
    pub fn eql(self: @This(), a: DeclarationLookup, b: DeclarationLookup) bool {
        _ = self;
        return a.scope_idx == b.scope_idx and std.mem.eql(u8, a.name, b.name);
    }
};

pub const DeclarationUseSite = struct {
    idx: usize,
    node_idx: Ast.Node.Index,
};

pub const FieldUseSite = struct {
    idx: usize,
    node_idx: Ast.Node.Index,
};

// memoized_resolved: std.AutoHashMap(Ast.Node.Index, Value) = .{},

// TODO: Add more scopes
pub const Scope = struct {
    node_idx: Ast.Node.Index,
    parent_scope: usize,

    pub const ScopeKind = enum { container, block, function };
    pub fn scopeKind(scope: Scope, tree: Ast) ScopeKind {
        return switch (tree.nodes.items(.tag)[scope.node_idx]) {
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
            => .container,
            else => .block,
        };
    }

    pub fn getLabel(scope: Scope, tree: Ast) ?Ast.TokenIndex {
        const token_tags = tree.tokens.items(.tag);

        return switch (scope.scopeKind(tree)) {
            .block => z: {
                const lbrace = tree.nodes.items(.main_token)[scope.node_idx];
                break :z if (token_tags[lbrace - 1] == .colon and token_tags[lbrace - 2] == .identifier)
                    lbrace - 2
                else
                    null;
            },
            else => null,
        };
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
                for (s.declarations.items) |di| {
                    const decl = value.unit.declarations.items[di];
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

pub const ParentScopeIterator = struct {
    unit: *SourceUnit,
    scope_idx: usize,

    pub fn next(psi: *ParentScopeIterator) ?usize {
        if (psi.scope_idx == std.math.maxInt(usize)) return null;
        const curr = psi.scope_idx;
        psi.scope_idx = psi.unit.scopes.items[psi.scope_idx].parent_scope;
        return curr;
    }
};

pub fn parentScopeIterator(unit: *SourceUnit, scope_idx: usize) ParentScopeIterator {
    return ParentScopeIterator{ .unit = unit, .scope_idx = scope_idx };
}

pub const InterpretResult = union(enum) {
    @"break": ?[]const u8,
    break_with_value: struct {
        label: ?[]const u8,
        value: tv.Value,
    },
    value: tv.Value,
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
    parent_scope_idx: ?usize,
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
            // TODO: Handle non-structs
            try unit.scopes.append(unit.allocator, .{
                .node_idx = node_idx,
                .parent_scope = parent_scope_idx orelse std.math.maxInt(usize),
            });
            const scope_idx = unit.scopes.items.len - 1;

            var type_info = tv.TypeInfo{
                .@"struct" = .{},
            };

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
                    var init_type = try unit.interpret(field_info.ast.type_expr, scope_idx, .{ .observe_values = true, .is_comptime = true });
                    const field = tv.Field{
                        .node_idx = member,
                        .name = tree.tokenSlice(field_info.ast.name_token),
                        .container_scope_idx = scope_idx,
                        .@"type" = init_type.getValue().value_data.@"type",
                        // TODO: Default values
                        // .@"type" = T: {
                        //     var value = (try unit.interpret(field_info.ast.type_expr, scope_idx, true)).?.value;
                        //     break :T @ptrCast(*tv.Type, @alignCast(@alignOf(*tv.Type), value)).*;
                        // },
                        // .value = null,
                    };

                    try unit.fields.append(unit.allocator, field);
                    try type_info.@"struct".fields.append(unit.allocator, unit.fields.items.len - 1);
                } else {
                    _ = try unit.interpret(member, scope_idx, options);
                    switch (tags[member]) {
                        .global_var_decl,
                        .local_var_decl,
                        .aligned_var_decl,
                        .simple_var_decl,
                        => {
                            try type_info.@"struct".declarations.append(unit.allocator, unit.declarations.items.len - 1);
                        },
                        else => {},
                    }
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
            var value = (try unit.interpret(decl.ast.init_node, parent_scope_idx, options)).getValue();
            var @"type" = if (decl.ast.type_node == 0) tv.Value{
                .node_idx = std.math.maxInt(Ast.Node.Index),
                .@"type" = try unit.createType(node_idx, .{ .@"type" = .{} }),
                .value_data = .{ .@"type" = value.@"type" },
            } else (try unit.interpret(decl.ast.type_node, parent_scope_idx, options)).getValue();

            try unit.addDeclaration(parent_scope_idx.?, .{
                .node_idx = node_idx,
                .name = utils.getDeclName(tree, node_idx).?,
                .scope_idx = parent_scope_idx.?, // orelse std.math.maxInt(usize),
                .@"value" = value,
                .@"type" = @"type".value_data.@"type",
            });

            return InterpretResult{ .nothing = .{} };
        },
        .block,
        .block_semicolon,
        .block_two,
        .block_two_semicolon,
        => {
            try unit.scopes.append(unit.allocator, .{
                .node_idx = node_idx,
                .parent_scope = parent_scope_idx orelse std.math.maxInt(usize),
            });
            const scope_idx = unit.scopes.items.len - 1;

            var buffer: [2]Ast.Node.Index = undefined;
            const statements = utils.blockStatements(tree, node_idx, &buffer).?;

            for (statements) |idx| {
                const ret = try unit.interpret(idx, scope_idx, options);
                switch (ret) {
                    .@"break" => |lllll| {
                        const maybe_block_label_string = if (unit.scopes.items[scope_idx].getLabel(tree)) |i| tree.tokenSlice(i) else null;
                        if (lllll) |l| {
                            if (maybe_block_label_string) |ls| {
                                if (std.mem.eql(u8, l, ls)) {
                                    return InterpretResult{ .nothing = .{} };
                                } else return ret;
                            } else return ret;
                        } else {
                            return InterpretResult{ .nothing = .{} };
                        }
                    },
                    .break_with_value => |bwv| {
                        const maybe_block_label_string = if (unit.scopes.items[scope_idx].getLabel(tree)) |i| tree.tokenSlice(i) else null;

                        if (bwv.label) |l| {
                            if (maybe_block_label_string) |ls| {
                                if (std.mem.eql(u8, l, ls)) {
                                    return InterpretResult{ .value = bwv.value };
                                } else return ret;
                            } else return ret;
                        } else {
                            return InterpretResult{ .value = bwv.value };
                        }
                    },
                    else => {},
                }
            }

            return InterpretResult{ .nothing = .{} };
        },
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

            var psi = unit.parentScopeIterator(parent_scope_idx.?);
            while (psi.next()) |i| {
                if (unit.declaration_map.get(.{ .scope_idx = i, .name = value })) |decl_idx| {
                    return InterpretResult{ .value = unit.declarations.items[decl_idx].value };
                }
            }

            std.log.err("Identifier not found: {s}", .{value});
            @panic("Could not find identifier");
        },
        .grouped_expression => {
            return try unit.interpret(data[node_idx].lhs, parent_scope_idx, options);
        },
        .@"break" => {
            const label = if (data[node_idx].lhs == 0) null else tree.tokenSlice(data[node_idx].lhs);
            return if (data[node_idx].rhs == 0)
                InterpretResult{ .@"break" = label }
            else
                InterpretResult{ .break_with_value = .{ .label = label, .value = (try unit.interpret(data[node_idx].rhs, parent_scope_idx, options)).getValue() } };
        },
        .@"if", .if_simple => {
            const iff = utils.ifFull(tree, node_idx);
            if (options.observe_values) {
                const ir = try unit.interpret(iff.ast.cond_expr, parent_scope_idx, options);
                if (ir.getValue().value_data.@"bool") {
                    return try unit.interpret(iff.ast.then_expr, parent_scope_idx, options);
                } else {
                    if (iff.ast.else_expr != 0) {
                        return try unit.interpret(iff.ast.else_expr, parent_scope_idx, options);
                    } else return InterpretResult{ .nothing = .{} };
                }
            } else {
                _ = try unit.interpret(iff.ast.cond_expr, parent_scope_idx, options);
                _ = try unit.interpret(iff.ast.then_expr, parent_scope_idx, options);
                _ = try unit.interpret(iff.ast.else_expr, parent_scope_idx, options);
            }
            @panic("bruh");
        },
        .equal_equal => {
            var a = try unit.interpret(data[node_idx].lhs, parent_scope_idx, options);
            var b = try unit.interpret(data[node_idx].rhs, parent_scope_idx, options);
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

                var psi = unit.parentScopeIterator(parent_scope_idx.?);
                while (psi.next()) |i| {
                    if (unit.declaration_map.get(.{ .scope_idx = i, .name = value })) |decl_idx| {
                        unit.declarations.items[decl_idx].value = (try unit.interpret(data[node_idx].rhs, i, options)).getValue();
                        // TODO: Register declaration use site
                    }
                }

                return InterpretResult{ .nothing = .{} };
            } else {
                const value = tree.getNodeSource(data[node_idx].lhs);

                var psi = unit.parentScopeIterator(parent_scope_idx.?);
                while (psi.next()) |i| {
                    if (unit.declaration_map.get(.{ .scope_idx = i, .name = value })) |_| {
                        _ = try unit.interpret(data[node_idx].lhs, i, options);
                        _ = try unit.interpret(data[node_idx].rhs, i, options);
                        // TODO: Register declaration use site
                    }
                }

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
                    const res = (try unit.interpret(param, parent_scope_idx, .{ .observe_values = true, .is_comptime = true })).getValue();
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
            return try unit.interpret(data[node_idx].lhs, parent_scope_idx, .{ .observe_values = true, .is_comptime = true });
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
