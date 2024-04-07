*** Schmidt, Lee, and Mitchell, "Climate Bones of Contention" 

** Table 2: Onset
use "SLM_Onset_19012001.dta" 

*** Model 1
logit terriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

*** Model 2
logit riveriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

*** Model 3
logit mariss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

*** Model 4
logit onset c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

*** Model 5
logit terriss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

*** Model 6
logit riveriss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

*** Model 7
logit mariss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

*** Model 8
logit onset vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
	cap_relative joint_dem lgdist iss_peaceyr, robust
	
** Table 3: Militarization
use "SLM_Militarization_19012001.dta", clear

*** Model 1 
logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal   ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1, robust

*** Model 2
logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1, robust

*** Model 3
logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1, robust

*** Model 4
logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr, robust

*** Model 5
logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1, robust

*** Model 6
logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1, robust

*** Model 7
logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1, robust

*** Model 8
logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr, robust
