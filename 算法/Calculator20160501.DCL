//Caculator.dcl
Calculator : dialog {
  label = "CAD计算器 Ver 1.0";
  initial_focus = "Expression";
  : boxed_column {
    children_alignment = right;
    errtile;
    : edit_box {
      key = "Expression";
      action = "(CAL:Expression $value)";	//编辑算式动作
      height = 2;
      fixed_height = true;
    }
    : row {
      : text {
        key = "TextPrec";
        value = "精度: ";
      }
      : slider {
        key = "Precision";
        action = "(CAL:Precision $value)";	//改变精度动作
        min_value = 0;
        max_value = 20;
        value = 8;
        width = 44;
        fixed_width = true;
      }
    }
    //spacer;
    
    : row {
      : button {
        label = "x\u+00b2";
        key = "sqr";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "x\u+00b3";
        key = "cube";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "\u+221A";
        key = "sqrt";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "\u+00b3\u+221A";
        key = "cuberoot";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "abs";
        key = "abs";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
    }
    
    : row {
      : button {
        label = "sin";
        key = "sin";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "cos";
        key = "cos";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "tan";
        key = "tan";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "ln";
        key = "ln";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "log";
        key = "log10";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
    }
    : row {
      : button {
        label = "asin";
        key = "asin";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "acos";
        key = "acos";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "atan";
        key = "atan";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "e^x";
        key = "exp";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "10^x";
        key = "exp10";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
    }

    : row {
      : button {
        label = "r2d";
        key = "r2d";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "d2r";
        key = "d2r";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "rnd";
        key = "round";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "Int";
        key = "fix";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
      : button {
        label = "float";
        key = "float";
        width = 10;
        fixed_width = true;
        height = 2;
        fixed_height = true;
      }
    }
    spacer;
    
    : row {
      : button {
        label = "(";
        key = "(";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = ")";
        key = ")";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "E";
        key = "2.71828182845904523536";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "Pi";
        key = "pi";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "Phi";
        key = "0.61803398874989484820";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
    }
        
    : row {
      : button {
        label = "\u+2190";
        key = "backspace";
        action = "(CAL:backspace)" ;
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "CE";
        key = "correct";
        action = "(CAL:correct)" ;
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "C";
        key = "clear";
        action = "(CAL:clear)" ;
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "\u+00b1";
        key = "RevSign";
        action = "(CAL:RevSign)" ;
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "x^y";
        key = "^";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
    }
        
    : row {
      : button {
        label = "7";
        key = "7";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "8";
        key = "8";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "9";
        key = "9";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "/";
        key = "/";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "%";
        key = "%";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
    }
        
    : row {
      : button {
        label = "4";
        key = "4";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "5";
        key = "5";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "6";
        key = "6";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "*";
        key = "*";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
      : button {
        label = "1/x";
        key = "reciprocal";
        action = "(CAL:reciprocal)";
        width = 10;
        fixed_width = true;
        height = 2.5;
        fixed_height = true;
      }
    }

    : row {
      : column {
        : row {
          : button {
            label = "1";
            key = "1";
            width = 10;
            fixed_width = true;
            height = 2.5;
            fixed_height = true;
          }
          : button {
            label = "2";
            key = "2";
            width = 10;
            fixed_width = true;
            height = 2.5;
            fixed_height = true;
          }
          : button {
            label = "3";
            key = "3";
            width = 10;
            fixed_width = true;
            height = 2.5;
            fixed_height = true;
          }
          : button {
            label = "-";
            key = "-";
            width = 10;
            fixed_width = true;
            height = 2.5;
            fixed_height = true;
          }
        }

        : row {
          : button {
            label = "0";
            key = "0";
            width = 21.4;
            fixed_width = true;
            height = 2.5;
            fixed_height = true;
          }
          : button {
            label = ".";
            key = ".";
            width = 10;
            fixed_width = true;
            height = 2.5;
            fixed_height = true;
          }
          : button {
            label = "+";
            key = "+";
            width = 10;
            fixed_width = true;
            height = 2.5;
            fixed_height = true;
          }
        }
      }
      : button {
        label = "=";
        key = "=";
        action = "(CAL:GetAnswer)";
        width = 10;
        fixed_width = true;
        height = 5.5;
        fixed_height = true;
      }
    }
    spacer;
  }
  
  //与CAD交互区
  : row {
    : button {
      label = "拾取文本<";
      key = "pick";
      width = 18;
      fixed_width = true;
      height = 2;
      fixed_height = true;
    }
    : button {
      label = "改变文本<";
      key = "change";
      width = 18;
      fixed_width = true;
      height = 2;
      fixed_height = true;
    }
    : button {
      label = "批量修改<";
      key = "batch";
      width = 18;
      fixed_width = true;
      height = 2;
      fixed_height = true;
    } 
  }

  //版权信息
  : text
  {
    key = "copyright1";
    alignment = centered;
    value = "Copyright 2006-2016 Highflybird \u+00AE All copyrights reserved.";
    is_bold = true;
  }
  : text
  {
    key = "copyright2";
    alignment = centered;
    value = "Made in Shenzhen,China,2016.04.20. Email:highflybird@qq.com.";
    is_bold = true;
  }
  
  ok_cancel_help;
}  