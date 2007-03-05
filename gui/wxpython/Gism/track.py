class Track:
	"""
	This class has functions and variables for tracking map display,
	associated notebook pages, and other index values.
	"""
	def __init__(self):
		"""
		Variables set at initialization
		"""
		global layertree
		global nb
		global cb_page
		global cb_pgnum
		global curr_disp
		global disp_idx
		global disp_ctrl
#
	# store and retrieve index of display with focus
	def SetDisp_idx(self, idx):
		global disp_idx
		disp_idx = idx
		return disp_idx

	def GetDisp_idx(self):
		global disp_idx
		return disp_idx

	#store and retrieve current display with focus
	def SetDisp(self, disp, disp_idx):
		curr_disp[disp_idx] = disp
		return curr_disp[disp_idx]

	def GetDisp(self, disp_idx):
		return curr_disp[disp_idx]

	# store and retrieve choicebook widget that hold all GIS manager controls
	def SetChbk(self, cb):
		gm_cb = cb
		return gm_cb

	def GetChbk(self):
		return gm_cb

	#store and retrieve display index for choicebook page
	def SetCB_idx(self, page, disp_idx):
		cb_page[page] = disp_idx
		return cb_page[page]

	def GetCB_idx(self, page):
		return cb_page[page]

	#store and retrieve page number of choicebook page
	def SetCB_page(self, page , pgnum):
		cb_pgnum[page] = pgnum
		return cb_pgnum[page]

	def GetCB_page(self, page):
		return cb_pgnum[page]

	def SetDispCtrl(self, disp_idx, disp, chbk):
		disp_ctrl[disp_idx] = (disp, chbk)
		return disp_ctrl[disp_idx]

	def GetDispCtrl(self, disp_idx):
		return disp_ctrl[disp_idx]

	#store and retriev notebook with layer tree and console for display with focus
	def SetNB(self, disp_idx, notebk):
		nb[disp_idx] = notebk
		return nb[disp_idx]

	def GetNB(self, disp_idx):
		return nb[disp_idx]

	#store and retriev notebook with layer tree and console for display with focus
	def SetTree(self, disp_idx, tree):
		layertree[disp_idx] = tree
		return layertree[disp_idx]

	def GetTree(self, disp_idx):
		return layertree[disp_idx]

