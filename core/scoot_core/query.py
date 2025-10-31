from .opcontext import OperationContext
from .metadata import try_describe_table
from .model import ResultSet, TableModel

from sqlglot import parse_one, exp


class SQLQueryModifier:
    def __init__(self, sql, ctx):
        self.ast = parse_one(sql)
        self.ctx = ctx
        self._tbl_expr_meta: dict[str, TableModel] = {}
        self._identify_items()

    def remove_from_select(self, items):
        select_clause = self.ast.find(exp.Select)

        if select_clause is None:
            return

        keep = []
        expr_items = self._find_items_in_expression(items)

        for item in select_clause.expressions:
            if isinstance(item, exp.Star):
                table_meta = self._get_table_expr_metadata()
                for tbl_name, tbl in table_meta.items():
                    tbl_expr = self._find_table_expression({"table": tbl_name})
                    for col in tbl.columns:
                        if col.name not in [i.get("name") for i in items]:
                            if tbl_expr and tbl_expr.get("alias"):
                                keep.append(
                                    exp.Column(
                                        this=exp.Identifier(this=col.name),
                                        table=exp.Identifier(
                                            this=tbl_expr.get("alias")
                                        ),
                                    )
                                )
                            else:
                                keep.append(
                                    exp.Column(this=exp.Identifier(this=col.name))
                                )

            elif isinstance(item, exp.Column):
                if item.sql() not in [expr.get("sql") for expr in expr_items]:
                    keep.append(item)

        if not keep:
            keep = [exp.Star()]

        select_clause.set("expressions", keep)

    def add_where_condition(self, condition):
        where_clause = self.ast.find(exp.Where)

        where_node = parse_one(f"SELECT * FROM x WHERE {condition}").find(exp.Where)
        new_condition = where_node.this if where_node else None

        if where_clause:
            where_clause.set(
                "this", exp.And(this=where_clause.this, expression=new_condition)
            )
        else:
            self.ast.set("where", exp.Where(this=new_condition))

    def remove_where_condition(self, condition):
        where_clause = self.ast.find(exp.Where)

        if not where_clause:
            return

        # Parse the condition as an expression
        where_node = parse_one(f"SELECT * FROM x WHERE {condition}").find(exp.Where)
        if where_node is None:
            return

        target_expr = where_node.this

        # Recursively search and remove the target condition
        def remove_expr(node):
            if isinstance(node, exp.And):
                left = remove_expr(node.this)
                right = remove_expr(node.expression)

                # If both sides are None, the entire AND can be removed
                if left is None and right is None:
                    return None
                # If only one side is None, return the other side
                elif left is None:
                    return right
                elif right is None:
                    return left
                # Otherwise, update the AND node
                node.set("this", left)
                node.set("expression", right)
                return node

            # Direct comparison for leaf nodes
            if node == target_expr:
                return None

            return node
            if isinstance(node, exp.And):
                if node.this == target_expr:
                    return node.expression
                elif node.expression == target_expr:
                    return node.this
                else:
                    node.set("this", remove_expr(node.this))
                    node.set("expression", remove_expr(node.expression))
                    return node
            return node

        new_where = remove_expr(where_clause.this)
        if isinstance(new_where, exp.And):
            self.ast.set("where", exp.Where(this=new_where))
        elif new_where:
            self.ast.set("where", exp.Where(this=new_where))
        else:
            self.ast.set("where", None)

    def _identify_owner(self, item):
        for tbl_expr in self._tbl_expr:
            if item.get("owner") == tbl_expr.get("alias"):
                return tbl_expr
            elif item.get("owner") == tbl_expr.get("table"):
                return tbl_expr

    def _identify_items(self):
        from_clause = self.ast.find(exp.From)

        table_expressions = []
        select_items = []

        if from_clause:
            from_value = from_clause.args.get("this")
            # if isinstance(from_value, tuple):
            #     for expr in from_value:
            #         if isinstance(expr, exp.Table):
            #             print(f"{expr}")
            if isinstance(from_value, exp.Table):
                table_expr = {}
                if from_value.name:
                    table_expr["table"] = from_value.name
                if from_value.alias:
                    table_expr["alias"] = from_value.alias
                if from_value.db:
                    table_expr["space"] = from_value.db

                table_expressions.append(table_expr)

        self._tbl_expr = table_expressions

        select_clause = self.ast.find(exp.Select)
        if select_clause:
            for item in select_clause.expressions:
                col = None
                alias = None
                if isinstance(item, exp.Column):
                    col = item
                elif isinstance(item, exp.Alias):
                    aliased_expr = item.args.get("this")
                    if isinstance(aliased_expr, exp.Column):
                        col = aliased_expr
                        alias = getattr(item.args.get("alias", {}), "this", None)

                if col:
                    item_expr = {}
                    if col.name:
                        item_expr["column"] = col.name
                    item_expr["sql"] = item.sql()
                    if alias:
                        item_expr["alias"] = alias
                    if col.table:
                        item_expr["owner"] = col.table
                    table = self._identify_owner(item_expr)
                    if table:
                        item_expr["table"] = table
                    select_items.append(item_expr)

        self._select_items = select_items

    def _find_items_in_expression(self, items: list[dict[str, str]]):
        found = []

        for item in items:
            for expr in self._select_items:
                if expr.get("column") == item.get("name"):
                    found.append(expr)

        return found

    def _find_table_expression(self, table_desc):
        for tbl_expr in self._tbl_expr:
            if tbl_expr.get("table") == table_desc.get("table"):
                return tbl_expr
        return None

    def _get_table_expr_metadata(self) -> dict[str, TableModel]:
        from_clause = self.ast.find(exp.From)
        if from_clause is None:
            return {}

        tables: list[exp.Table] = []

        from_value = from_clause.args.get("this")
        if isinstance(from_value, tuple):
            for item in from_value:
                if isinstance(item, exp.Table):
                    tables.append(item)
        elif isinstance(from_value, exp.Table):
            tables.append(from_value)

        for tbl_expr in tables:
            tbl_meta = try_describe_table(
                self.ctx, tbl_expr.sql()
            )
            if tbl_meta:
                self._tbl_expr_meta[tbl_meta.name] = tbl_meta

        return self._tbl_expr_meta

    def get_sql(self):
        return self.ast.sql()


def modify(ctx: OperationContext, sql: str, action_instr: dict) -> ResultSet:
    target = action_instr.get("target")
    operation = action_instr.get("operation")
    conditions = action_instr.get("conditions", [])

    query_mod = SQLQueryModifier(sql, ctx.connection)

    if target == "WHERE":
        for cond in conditions:
            lhs = cond.get("lhs")
            cmp = cond.get("cmp")
            rhs = cond.get("rhs")

            expr = f"{lhs} {cmp} {rhs}"

            if operation == "add":
                query_mod.add_where_condition(expr)
            elif operation == "remove":
                query_mod.remove_where_condition(expr)
            else:
                raise ValueError(f"Unknown operation: {operation}")
    elif target == "SELECT":
        if operation == "add":
            query_mod.add_to_select(conditions)
        elif operation == "remove":
            query_mod.remove_from_select(conditions)
        else:
            raise ValueError(f"Unknown operation: {operation}")
    else:
        raise ValueError(f"Unknown query modification target: {target}")

    modified_sql = query_mod.get_sql()

    result_set = ctx.connection.execute(modified_sql)
    result_set.metadata = (result_set.metadata or {}) | {"stmt": modified_sql}
    return result_set

def execute(ctx: OperationContext, sql: str):
    return perform_action(ctx, sql, { "action": "execute"})

def perform_action(ctx: OperationContext, sql: str, action_instr: dict):
    """Perform an action on an SQL query.

    Args:
            connection (Connection): The database connection.
            sql (str): The SQL query to execute.
            action (dict): The action to perform on the SQL query.
    """
    action = action_instr.get("action")
    with ctx.operation(action):
      if action == "execute":
          return ctx.connection.execute(sql)
      elif action == "modify":
          return modify(ctx, sql, action_instr)
      else:
          raise ValueError(f"Unknown action: {action}")

