const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const utils = @import("utils.zig");
const SourceUnit = @import("SourceUnit.zig");

pub const TypeInfo = union(enum) {
    pub const Signedness = enum { signed, unsigned };

    pub const Struct = struct {
        /// Index into unit.fields
        fields: std.ArrayListUnmanaged(usize) = .{},
        /// Index into unit.declarations
        declarations: std.ArrayListUnmanaged(usize) = .{},
    };

    pub const Int = struct {
        bits: u16,
        signedness: Signedness,
    };

    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        is_volatile: bool,
        child: Type,
        is_allowzero: bool,

        sentinel: ?ValueData,

        pub const Size = enum {
            one,
            many,
            slice,
            c,
        };
    };

    pub const Fn = struct {
        return_type: ?Type,
        /// Index into unit.declarations
        params: std.ArrayListUnmanaged(usize) = .{},
    };

    /// Hack to get anytype working; only valid on fnparams
    @"anytype",
    @"type",
    @"bool",

    @"struct": Struct,
    pointer: Pointer,

    int: Int,
    @"comptime_int",
    float: u16,
    @"comptime_float",

    pub fn eql(source_unit: SourceUnit, a: TypeInfo, b: TypeInfo) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .@"struct" => false, // Struct declarations can never be equal
            .pointer => p: {
                const ap = a.pointer;
                const bp = b.pointer;
                break :p ap.size == bp.size and ap.is_const == bp.is_const and ap.is_volatile == bp.is_volatile and eql(
                    source_unit,
                    source_unit.type_info.items[ap.child.info_idx],
                    source_unit.type_info.items[bp.child.info_idx],
                ) and ap.is_allowzero == bp.is_allowzero and ((ap.sentinel == null and bp.sentinel == null) or ((ap.sentinel != null and bp.sentinel != null) and ap.sentinel.?.eql(bp.sentinel.?)));
            },
            .int => a.int.signedness == b.int.signedness and a.int.bits == b.int.bits,
            .float => a.float == b.float,
            else => return true,
        };
    }

    pub fn hash(context: SourceUnit.TypeInfoContext, ti: TypeInfo) void {
        context.hasher.update(&[_]u8{@enumToInt(ti)});
        return switch (ti) {
            .@"struct" => |s| {
                context.hasher.update(std.mem.sliceAsBytes(s.fields.items));
                context.hasher.update(std.mem.sliceAsBytes(s.declarations.items));
            },
            .pointer => |p| {
                // const ap = a.pointer;
                // const bp = b.pointer;
                context.hasher.update(&[_]u8{ @enumToInt(p.size), @boolToInt(p.is_const), @boolToInt(p.is_volatile) });
                TypeInfo.hash(context, context.unit.type_info.items[p.child.info_idx]);
                context.hasher.update(&[_]u8{@boolToInt(p.is_allowzero)});
                // TODO: Hash Sentinel
                // break :p ap.size == bp.size and ap.is_const == bp.is_const and ap.is_volatile == bp.is_volatile and eql(
                //     source_unit,
                //     source_unit.type_info.items[ap.child.info_idx],
                //     source_unit.type_info.items[bp.child.info_idx],
                // ) and ap.is_allowzero == bp.is_allowzero and ((ap.sentinel == null and bp.sentinel == null) or ((ap.sentinel != null and bp.sentinel != null) and ap.sentinel.?.eql(bp.sentinel.?)));
            },
            .int => |i| {
                // a.int.signedness == b.int.signedness and a.int.bits == b.int.bits;
                context.hasher.update(&[_]u8{@enumToInt(i.signedness)});
                context.hasher.update(&std.mem.toBytes(i.bits));
            },
            .float => |f| context.hasher.update(&std.mem.toBytes(f)),
            else => {},
        };
    }
};

pub const Type = struct {
    node_idx: Ast.Node.Index,
    info_idx: usize,
};

pub const Value = struct {
    node_idx: Ast.Node.Index,
    @"type": Type,
    value_data: ValueData,

    pub fn eql(value: Value, other_value: Value) bool {
        return value.value_data.eql(other_value.value_data);
    }
};

pub const ValueData = union(enum) {
    // TODO: Support larger ints, floats; bigints?

    @"type": Type,
    @"bool": bool,

    // @"struct": struct {

    // },
    // one_ptr: *anyopaque,
    /// TODO: Optimize this with an ArrayList that uses anyopaque slice
    slice_ptr: std.ArrayListUnmanaged(ValueData),

    @"comptime_int": std.math.big.int.Managed,
    unsigned_int: u64,
    signed_int: i64,
    float: f64,

    pub fn eql(data: ValueData, other_data: ValueData) bool {
        if (std.meta.activeTag(data) != std.meta.activeTag(other_data)) return false;
        // std.enums.
        // std.meta.activeTag(u: anytype)
        switch (data) {
            .@"bool" => return data.@"bool" == other_data.@"bool",
            .@"comptime_int" => return data.@"comptime_int".eq(other_data.@"comptime_int"),
            .unsigned_int => return data.unsigned_int == other_data.unsigned_int,
            .signed_int => return data.signed_int == other_data.signed_int,
            .float => return data.float == other_data.float,
            else => @panic("Simple eql not implemented!"),
        }
    }
};

pub const Field = struct {
    node_idx: Ast.Node.Index,
    /// Store name so tree doesn't need to be used to access field name
    name: []const u8,
    container_scope_idx: usize,
    @"type": Type,
    // default_value: ?Value,
};

pub const Declaration = struct {
    node_idx: Ast.Node.Index,
    /// Store name so tree doesn't need to be used to access declaration name
    name: []const u8,
    scope_idx: usize,
    @"type": Type,
    value: Value,

    // TODO: figure this out
    // pub const DeclarationKind = enum{variable, function};
    // pub fn declarationKind(declaration: Declaration, tree: Ast) DeclarationKind {
    //     return switch(tree.nodes.items(.tag)[declaration.node_idx]) {
    //         .fn_proto,
    //     .fn_proto_one,
    //     .fn_proto_simple,
    //     .fn_proto_multi,
    //     .fn_decl
    //     }
    // }

    pub fn isConstant(declaration: Declaration, tree: Ast) bool {
        return switch (tree.nodes.items(.tag)[declaration.node_idx]) {
            .global_var_decl,
            .local_var_decl,
            .aligned_var_decl,
            .simple_var_decl,
            => {
                return tree.tokenSlice(utils.varDecl(tree, declaration.node_idx).?.ast.mut_token).len == 3;
            },
            else => false,
        };
    }
};
