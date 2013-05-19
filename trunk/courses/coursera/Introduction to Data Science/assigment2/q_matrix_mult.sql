select a.row_num, b.col_num, sum(a.value * b.value) from A a, B b where a.col_num = b.row_num group by a.row_num, b.col_num;

