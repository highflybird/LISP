;;;==========SubFunction==========
;;;def _lx-ini-load 0
(00000000 20 2 1 1 0)
(00000005 100 1		UNVar1)
(00000007 5 0		UnVar0)
(00000009 3 6 0		*exe-dir*)
(0000000C 9 5 0		'("" ".FSL" ".LSP"))
(0000000F 2		T)
(00000010 81 4 4 0 1 0		find-file)
(00000016 8 1		UnVar1)
(00000018 5 1		UnVar1)
(0000001A 13 9 0		nil->JMP 00000026)
(0000001D 5 1		UnVar1)
(0000001F 81 1 3 0 1 0		load)
(00000025 22		ret)
(00000026 9 2 0		"init file not found:")
(00000029 5 0		UnVar0)
(0000002B 81 2 1 0 1 0		_msg-err)
(00000031 22		ret)
;;;==========Main==========
(00000000 20 0 0 0 0)
(00000005 1		nil)
(00000006 10)
(00000007 1		nil)
(00000008 10)
(00000009 9 3 0		'_lx-ini-load)
(0000000C 26 2 0		_lx-ini-load)
(0000000F 9 2 0		'_lx-ini-load)
(00000012 10)
(00000013 9 1 0		"VLINIT")
(00000016 53 1 2 0 1 32		_lx-ini-load)
(0000001C 22		ret)