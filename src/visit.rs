/// AST visitor trait for traversing `OpenSCAD` syntax trees.
///
/// Implement this trait to walk the AST without modifying it.
/// Each method has a default implementation that recurses into children.
use crate::ast::{Expr, ExprKind, SourceFile, Statement};

/// A visitor that traverses the AST by reference.
pub trait Visitor {
    fn visit_file(&mut self, file: &SourceFile) {
        for stmt in &file.statements {
            self.visit_statement(stmt);
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Include { .. } | Statement::Use { .. } | Statement::Empty { .. } => {}
            Statement::Assignment { expr, .. } => self.visit_expr(expr),
            Statement::ModuleDefinition { params, body, .. } => {
                for param in params {
                    if let Some(default) = &param.default {
                        self.visit_expr(default);
                    }
                }
                for stmt in body {
                    self.visit_statement(stmt);
                }
            }
            Statement::FunctionDefinition { params, body, .. } => {
                for param in params {
                    if let Some(default) = &param.default {
                        self.visit_expr(default);
                    }
                }
                self.visit_expr(body);
            }
            Statement::ModuleInstantiation { args, children, .. } => {
                for arg in args {
                    self.visit_expr(&arg.value);
                }
                for child in children {
                    self.visit_statement(child);
                }
            }
            Statement::IfElse {
                condition,
                then_body,
                else_body,
                ..
            } => {
                self.visit_expr(condition);
                for stmt in then_body {
                    self.visit_statement(stmt);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_statement(stmt);
                    }
                }
            }
            Statement::Block { body, .. } => {
                for stmt in body {
                    self.visit_statement(stmt);
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Number(_)
            | ExprKind::String(_)
            | ExprKind::BoolTrue
            | ExprKind::BoolFalse
            | ExprKind::Undef
            | ExprKind::Identifier(_) => {}
            ExprKind::UnaryOp { operand, .. } => self.visit_expr(operand),
            ExprKind::BinaryOp { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            ExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.visit_expr(condition);
                self.visit_expr(then_expr);
                self.visit_expr(else_expr);
            }
            ExprKind::FunctionCall { callee, args } => {
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(&arg.value);
                }
            }
            ExprKind::Index { object, index } => {
                self.visit_expr(object);
                self.visit_expr(index);
            }
            ExprKind::MemberAccess { object, .. } => self.visit_expr(object),
            ExprKind::Vector(elements) => {
                for elem in elements {
                    self.visit_expr(elem);
                }
            }
            ExprKind::Range { start, step, end } => {
                self.visit_expr(start);
                if let Some(s) = step {
                    self.visit_expr(s);
                }
                self.visit_expr(end);
            }
            ExprKind::Let { assignments, body }
            | ExprKind::LcLet { assignments, body }
            | ExprKind::LcFor {
                assignments, body, ..
            } => {
                for arg in assignments {
                    self.visit_expr(&arg.value);
                }
                self.visit_expr(body);
            }
            ExprKind::Assert { args, body } | ExprKind::Echo { args, body } => {
                for arg in args {
                    self.visit_expr(&arg.value);
                }
                if let Some(b) = body {
                    self.visit_expr(b);
                }
            }
            ExprKind::AnonymousFunction { params, body } => {
                for param in params {
                    if let Some(default) = &param.default {
                        self.visit_expr(default);
                    }
                }
                self.visit_expr(body);
            }
            ExprKind::LcForC {
                init,
                condition,
                update,
                body,
            } => {
                for arg in init {
                    self.visit_expr(&arg.value);
                }
                self.visit_expr(condition);
                for arg in update {
                    self.visit_expr(&arg.value);
                }
                self.visit_expr(body);
            }
            ExprKind::LcIf {
                condition,
                then_expr,
                else_expr,
            } => {
                self.visit_expr(condition);
                self.visit_expr(then_expr);
                if let Some(e) = else_expr {
                    self.visit_expr(e);
                }
            }
            ExprKind::LcEach { body } => self.visit_expr(body),
        }
    }
}
