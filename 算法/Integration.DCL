//Integration.dcl
dcl_Integration : dialog {
  label = "数值积分程序";
  : boxed_column {
    : row {
      : edit_box {
        key="F";
        label= "数学表达式:";
      }
    }
    : row {
      : edit_box {
        key="X1";
        label= "区间下届:";
        edit_width = 14;
      }
      : edit_box {
        key="X2";
        label= "区间上届:";
        edit_width = 14;
      }      
      : edit_box {
        key="N";
        label= "精确位数:";
        edit_width = 2;
      }
    }
    spacer_1;
  }
  : boxed_column {
    :row {
      : button {
        key = "S1";
        label = "龙贝格积分法";
      }
      : text {
      	label = "结果:";
      }
      : text {
        key = "R1";
        width = 20;
      }
    }
    :row {
      : button {
        key = "S2";
        label = "辛普森积分法";
      }
      : text {
      	label = "结果:";
      }
      : text {
        key = "R2";
        width = 20;
      }
    }
    :row {
      : button {
        key = "S3";
        label = "自适应积分法";
      }
      : text {
      	label = "结果:";
      }
      : text {
        key = "R3";
        width = 20;
      }
    }
    :row {
      : button {
        key = "S4";
        label = "变步长积分法";
      }
      : text {
      	label = "结果:";
      }
      : text {
        key = "R4";
        width = 20;
      }
    }
    spacer_1;
  }
  ok_cancel_help;//errtile;
  : text {
    key = "info";
    label = "Copyright \u+00A9 2002-2019 Highflybird. All rights reserved.";
  }
}  