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
scopes: std.ArrayListUnmanaged(Scope) = .{},
fields: std.ArrayListUnmanaged(tv.Field) = .{},
declarations: std.ArrayListUnmanaged(tv.Declaration) = .{},
// memoized_resolved: std.AutoHashMap(Ast.Node.Index, Value) = .{},

// TODO: Add more scopes
pub const Scope = struct {
    node_idx: Ast.Node.Index,
    parent_scope: usize,

    pub const ScopeKind = enum { container, block };
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
        try writer.print("Type with typeInfo {any} @ node {d}", .{ value.unit.type_info.items[value.@"type".info_idx], value.@"type".node_idx });
    }
};

pub fn formatType(unit: *SourceUnit, @"type": tv.Type) TypeFormatter {
    return TypeFormatter{ .unit = unit, .@"type" = @"type" };
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
    try unit.type_info.append(unit.allocator, type_info);
    const info_idx = unit.type_info.items.len - 1;
    return tv.Type{ .node_idx = node_idx, .info_idx = info_idx };
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
        return result.maybeGetValue() orelse @panic("Attempted to get value from non-valie interpret result");
    }
};

pub fn interpret(
    unit: *SourceUnit,
    node_idx: Ast.Node.Index,
    parent_scope_idx: ?usize,
    /// Whether or not this interpretation cares about values;
    /// this is always true in type determination / comptime
    observe_values: bool,
) std.mem.Allocator.Error!InterpretResult {
    // _ = unit;
    // _ = node;
    // _ = observe_values;

    const tree = unit.tree;
    const tags = tree.nodes.items(.tag);
    const data = tree.nodes.items(.data);

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
            try unit.scopes.append(unit.allocator, .{
                .node_idx = node_idx,
                .parent_scope = parent_scope_idx orelse std.math.maxInt(usize),
            });
            const scope_idx = unit.scopes.items.len - 1;

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
                    std.log.info("Z", .{});
                    var intt = try unit.interpret(field_info.ast.type_expr, scope_idx, true);
                    std.log.info("Z {any}", .{intt});

                    const field = tv.Field{
                        .node_idx = member,
                        .container_scope_idx = scope_idx,
                        .@"type" = intt.getValue().value_data.@"type",
                        // .@"type" = T: {
                        //     var value = (try unit.interpret(field_info.ast.type_expr, scope_idx, true)).?.value;
                        //     break :T @ptrCast(*tv.Type, @alignCast(@alignOf(*tv.Type), value)).*;
                        // },
                        // .value = null,
                    };

                    try unit.fields.append(unit.allocator, field);
                } else {
                    // try analyzer.scopeIntermediate(scope_idx, member, name);
                }
            }

            return InterpretResult{ .nothing = .{} };
        },
        .global_var_decl,
        .local_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        => {
            var value = (try unit.interpret(utils.varDecl(tree, node_idx).?.ast.init_node, parent_scope_idx, observe_values)).getValue();
            var @"type" = (try unit.interpret(utils.varDecl(tree, node_idx).?.ast.type_node, parent_scope_idx, observe_values)).getValue();

            try unit.declarations.append(unit.allocator, .{
                .node_idx = node_idx,
                .scope_idx = parent_scope_idx orelse std.math.maxInt(usize),
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
                const ret = try unit.interpret(idx, scope_idx, observe_values);
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

            @panic("Unhandled identifier type");
        },
        .grouped_expression => {
            return try unit.interpret(data[node_idx].lhs, parent_scope_idx, observe_values);
        },
        .@"break" => {
            // var psi = unit.parentScopeIterator(parent_scope_idx.?);
            // while (psi.next()) |i| {
            //     var scope = unit.scopes.items[i];
            //     if (scope.getLabel(tree)) |label_idx|
            //         if (std.mem.eql(u8, tree.getNodeSource(data[node_idx].lhs), tree.tokenSlice(label_idx))) return unit.interpret(data[node_idx].rhs, parent_scope_idx, observe_values);
            // }
            // @panic("Found no break exit!");

            const label = if (data[node_idx].lhs == 0) null else tree.tokenSlice(data[node_idx].lhs);
            return if (data[node_idx].rhs == 0)
                InterpretResult{ .@"break" = label }
            else
                InterpretResult{ .break_with_value = .{ .label = label, .value = (try unit.interpret(data[node_idx].rhs, parent_scope_idx, observe_values)).getValue() } };
        },
        else => {
            std.log.err("Unhandled {any}", .{tags[node_idx]});
            return InterpretResult{ .nothing = .{} };
        },
    }
}
