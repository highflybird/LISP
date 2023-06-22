Equation : dialog {
  label = "方程曲线";
  : row {
    : column {
      spacer;
      : boxed_column {
        label = "输入区";
        width = 35;
        fixed_width = true;
        //height = 15;
        //fixed_height = true;
        : row {
          : text {
            key = "LableX";
            value = "Fx = ";
            is_enabled = false;
          }
          : edit_box {
            key ="Fx";
            width = 28;
            fixed_width = true;
            value = "x";
            is_enabled = false;
          }
        }
        : row {
          : text {
            key = "LableY";
            value = "Fy = ";
            
          }
          : edit_box {
            key = "Fy";
            width = 28;
            fixed_width = true;
            value = "x";
          }
        }
        
        : edit_box {
          key = "Lower";
          label = "最小值:   ";
          value = "-2";
          edit_width = 10;
        }
        
        : edit_box {
          key = "Upper";
          label = "最大值:   ";
          value = "2";
          edit_width = 10;
        }
      
        : edit_box {
          key = "Precision";
          label = "等分数:   ";
          value = "1000";
          edit_width = 10;
        }
        spacer;
      }
      : boxed_radio_column {
        key = "Type";
        value = "Normal";
        label = "方程类型";
        height = 5;
        fixed_height = true;
        : radio_button
        {
          label = "坐标方程";
          key = "Normal";
        }
        : radio_button
        {
          label = "参数方程";
          key = "Param";
        }
        : radio_button
        {
          //alignment = right;
          //children_alignment = centered;
          label = "极坐标方程";
          key = "Polar";
        }
        spacer;
      }
      : text {
        label = "常用方程:";
      }
      : popup_list {
        key = "Equations";
        list = "y=x\ny=x^2\ny=x^3\ny=e^x\ny=ln(x)\ny=sin(x)\ny=cos(x)\ny=tan(x)\n";
        value = 0;
      }
      spacer;
      : image {
        key = "Logo";
        width = 30;
        aspect_ratio = 1;
        fixed_width = true;
        fixed_height = true;
        alignment = centered;
        color = -15;
      }
    }
    //(action_tile "Lower" "(FC:lower $key $value $reason)")
    //(action_tile "Upper" "(FC:lower $key $value $reason)")
    //(action_tile "Fx" "(FC:GetExpression $key $value $reason)")
    //(action_tile "Fy" "(FC:GetExpression $key $value $reason)")
    //(action_tile "Precision" "(FC:precision $key $value $reason)")
    //(action_tile "Equations" "(FC:Equations $key $value $reason)")
    : column {
      : row {
        : button {
          label = "预览";
          width = 12;
          fixed_width = true;
          key = "Preview";
        }
        : button {
          label = "放大";
          width = 12;
          fixed_width = true;
          key = "ZoomIn";
        }
        : button {
          label = "缩小";
          width = 12;
          fixed_width = true;
          key = "ZoomOut";
        }
        : button {
          label = "窗口";
          width = 12;
          fixed_width = true;
          key = "ZoomWindow";
        }
        : button {
          label = "复位";
          width = 12;
          fixed_width = true;
          key = "ZoomExtend";
        }
        : button {
          label = "画图";
          width = 12;
          fixed_width = true;
          key = "Drawing";
        }
      }
      : image_button {
        key = "Picture";
        height = 40;
        aspect_ratio = 1;
        fixed_width = true;
        fixed_height = true;
        alignment = centered;
        color = 154;
      }
    }
  }
  //errtile;
  ok_cancel_help;
}
