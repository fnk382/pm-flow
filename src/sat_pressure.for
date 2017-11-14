*******************************************************
*   calculation of saturation water vapor pressure    *
*******************************************************

	subroutine sat_pressure(to,pvsat)
	implicit doubleprecision (a-h,o-z)
	doubleprecision c1,c2,c3,c4,c5,c6,c7
	doubleprecision c8,c9,c10,c11,c12,c13
	doubleprecision to,pvsat

	c1=-5.6745359d+03
	c2=6.3925247
	c3=-9.677843d-03
	c4=6.22115701d-07
	c5=2.0747825d-09
	c6=-9.484024d-13
	c7=4.1635019

	c8=-5.8002206d+03
	c9=1.3914993
	c10=-4.8640239d-02
	c11=4.1764768d-05
	c12=-1.4452093d-08
	c13=6.5459673

	if(to.gt.273.15) then
	 
	   pvsat=exp(c8/to+c9+c10*to+c11*to**2+c12*to**3+c13*
     *               log(to))
	 end if
	
	if (to.lt.273.15)then
	   pvsat=exp(c1/to+c2+c3*to+c4*to**2+c5*to**3+c6*to**4+
     *              c7*log(to))

	end if

	return
	end