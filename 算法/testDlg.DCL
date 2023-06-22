Test : dialog
{
	label = "对话框测试";
	:boxed_column
	{
		: button
		{
		      	key ="K1";
		      	label = "开始测试";
		}
		: button
		{
			key ="K2";
			label = "交互<";
		}
	}
	: image {
        key = "Logo";
        width = 30;
        aspect_ratio = 1;
        fixed_width = true;
        fixed_height = true;
        alignment = centered;
        color = -15;
      }
	ok_cancel;
}  