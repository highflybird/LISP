//solve.dcl
dcl_solve : dialog {
  label = "一元方程求解程序";
  key = "p0";
  : boxed_column {
    : row {
      key = "expression";
      : edit_box {
        key="p1";
        label= "一元方程:";
      }
    }
    : row {
      key = "F&A";
      fixed_width = true;
      : edit_box {
        key="p2";
        label= "区间下届:";
        edit_width = 8;
      }
      : edit_box {
        key="p3";
        label= "区间上届:";
        edit_width = 8;
      }      
      : edit_box {
        key="p4";
        label= "精确位数:";
        edit_width = 2;
      }
    }
    spacer_1;
  }
  ok_cancel_help;
  //errtile;
}  