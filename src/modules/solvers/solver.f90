module solver
  use params
  use oacc_params
  implicit none

  real(dp) :: rl, pl, ul, vl, wl, cl, bl, al
  real(dp) :: rr, pr, ur, vr, wr, cr, br, ar
  
  real(dp) :: entho, a, sgnm, ploc, proc
  real(dp) :: ecinl, emagl, etotl, ptotl, vdotbl, el, cfastl, calfvenl, rcl
  real(dp) :: ecinr, emagr, etotr, ptotr, vdotbr, er, cfastr, calfvenr, rcr

  !$acc declare create(ul, vl, wl, rl, bl, cl, entho, a, ploc, ecinl, emagl, etotl, ptotl, vdotbl)

contains
  subroutine riemann_solver(qm, qp, fgodunov, fgodunov_pre)
    use variables, only: dt, ds, dv, x, y
    implicit none
    
    real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2,nvar,ndim), intent(in) :: qm 
    real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2,nvar,ndim), intent(in) :: qp 
    real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2,nvar,ndim),intent(out) :: fgodunov
    real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2,ndim), intent(out) :: fgodunov_pre
    real(dp), dimension(:,:,:), allocatable :: rgstar, ugstar, vgstar, wgstar
    real(dp), dimension(:,:,:), allocatable :: bgstar, cgstar, pgstar
    real(dp) :: bn_mean
    !real(dp) :: rl, pl, ul, vl, wl, cl, bl, al
    !real(dp) :: rr, pr, ur, vr, wr, cr, br, ar
    real(dp) :: ro, uo, vo, wo, bo, co, ptoto, pressure
    real(dp) :: cotanxc, shear, xL, Ekin, Emag, Etot
    integer  :: i, j, k, idim, im, jm, km, ii, ji, ki, ioffset, joffset, koffset
    integer  :: ln, lt1, lt2, bn, bt1, bt2, ihip, jhip, khip
    integer  :: ilo, ihi, jlo, jhi, klo, khi

    real(dp) :: sl, sr, sal, sar
    !real(dp) :: entho, a, sgnm, ploc, proc
    !real(dp) :: ecinl, emagl, etotl, ptotl, vdotbl, el, cfastl, calfvenl, rcl
    !real(dp) :: ecinr, emagr, etotr, ptotr, vdotbr, er, cfastr, calfvenr, rcr
    real(dp) :: rstarl, vstarl, wstarl, bstarl, cstarl, vdotbstarl
    real(dp) :: rstarr, vstarr, wstarr, bstarr, cstarr, vdotbstarr
    real(dp) :: sqrrstarl, etotstarl, etotstarstarl
    real(dp) :: sqrrstarr, etotstarr, etotstarstarr
    real(dp) :: ustar, ptotstar, estar, vstarstar, wstarstar, bstarstar
    real(dp) :: cstarstar, vdotbstarstar
    real(dp) :: etoto, vdotbo
    real(dp) :: c2, b2, d2, cf

  if (verbose) print*, '> Entering riemann_hlld'

  allocate(rgstar(iu1:iu2,ju1:ju2,ku1:ku2))
  allocate(ugstar(iu1:iu2,ju1:ju2,ku1:ku2))
  allocate(vgstar(iu1:iu2,ju1:ju2,ku1:ku2))
  allocate(wgstar(iu1:iu2,ju1:ju2,ku1:ku2))
  allocate(bgstar(iu1:iu2,ju1:ju2,ku1:ku2))
  allocate(cgstar(iu1:iu2,ju1:ju2,ku1:ku2))
  allocate(pgstar(iu1:iu2,ju1:ju2,ku1:ku2))
  
#if OACC == 1
  !$acc kernels loop
  do k = ku1, ku2
     do j = ju1, ju2
        do i = iu1, iu2
           rgstar(i,j,k) = zero
           ugstar(i,j,k) = zero
           vgstar(i,j,k) = zero
           wgstar(i,j,k) = zero
           bgstar(i,j,k) = zero
           cgstar(i,j,k) = zero
           pgstar(i,j,k) = zero
        enddo
     enddo
  enddo
#else
  !$OMP PARALLEL WORKSHARE
  rgstar = zero; ugstar = zero; vgstar = zero; wgstar = zero
  bgstar = zero; cgstar = zero; pgstar = zero
  !$OMP END PARALLEL WORKSHARE
#endif

  !$acc data pcreate(rgstar, ugstar, vgstar, wgstar, bgstar, cgstar, pgstar)

  do idim = 1, ndim
     if(idim == 1) then
        ln = 2; lt1 = 3; lt2 = 4
        bn = 6; bt1 = 7; bt2 = 8
        ilo = if1          ; ihi = if2
        jlo = min(1, ju1+3); jhi = max(1, ju2-3)
        klo = min(1, ku1+3); khi = max(1, ku2-3)
        ioffset = 1; joffset = 0; koffset = 0
     else if(idim == 2) then
        ln = 3; lt1 = 2; lt2 = 4
        bn = 7; bt1 = 6; bt2 = 8
        ilo = min(1, iu1+3); ihi = max(1, iu2-3)
        jlo = jf1          ; jhi = jf2
        klo = min(1, ku1+3); khi = max(1, ku2-3)
        ioffset = 0; joffset = 1; koffset = 0
     else
        ln = 4; lt1 = 2; lt2 = 3
        bn = 8; bt1 = 6; bt2 = 7
        ilo = min(1, iu1+3); ihi = max(1, iu2-3)
        jlo = min(1, ju1+3); jhi = max(1, ju2-3)
        klo = kf1          ; khi = kf2
        ioffset = 0; joffset = 0; koffset = 1
     endif
     !$acc parallel loop private(ul, vl, wl, rl, bl, cl, entho, a, ploc, ecinl, emagl, etotl, ptotl, vdotbl)
     !$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(bn_mean, im, jm, km, shear) &
     !$OMP PRIVATE(Ekin, Emag, Etot, rl, pl, ul, vl, wl, cl, bl, al, rr, pr) &
     !$OMP PRIVATE(ur, vr, wr, cr, br, ar, ro, uo, vo, wo, bo, co, ptoto)

     do k = klo, khi
        !!$acc loop vector(blocky_solver)
        do j = jlo, jhi
           !!$acc loop vector(blockx_solver)
           do i = ilo, ihi
              im = i - ioffset
              jm = j - joffset
              km = k - koffset

              ! Enforce continuity for normal magnetic field
              bn_mean = half*(qm(im,jm,km,bn,idim) + qp(i,j,k,bn,idim))

              ! Left state
              rl = qm(im,jm,km,1  ,idim) ! Mass density
              pl = qm(im,jm,km,5  ,idim) ! Pressure
              ul = qm(im,jm,km,ln ,idim) ! Normal velocity
              al = bn_mean               ! Normal magnetic field
              vl = qm(im,jm,km,lt1,idim) ! Tangential velocity 1
              bl = qm(im,jm,km,bt1,idim) ! Tangential magnetic field 1
              wl = qm(im,jm,km,lt2,idim) ! Tangential velocity 2
              cl = qm(im,jm,km,bt2,idim) ! Tangential magnetic field 2
              
              ! Right state
              rr = qp(i,j,k,1  ,idim) ! Mass density
              pr = qp(i,j,k,5  ,idim) ! Pressure
              ur = qp(i,j,k,ln ,idim) ! Normal velocity
              ar = bn_mean            ! Normal magnetic field
              vr = qp(i,j,k,lt1,idim) ! Tangential velocity 1
              br = qp(i,j,k,bt1,idim) ! Tangential magnetic field 1
              wr = qp(i,j,k,lt2,idim) ! Tangential velocity 2
              cr = qp(i,j,k,bt2,idim) ! Tangential magnetic field 2

              if (iriemann == ihlld) then
                 !call hlld(qm, qp, fgodunov, rgstar, ugstar, vgstar, wgstar, bgstar, cgstar, pgstar, ro, uo, vo, wo, bo, co, ptoto, rl, pl, ul, vl, wl, cl, bl, al, rr, pr, ur, vr, wr, cr, br, ar)

                 entho = one/(gamma-one)
                 
                 ! enforce continuity of normal component
                 a    = half*(al + ar)
                 sgnm = sign(one, a)
                 al   = a; ar = a
                 
#if ISO == 1
                 ploc = rl*ciso**2
                 proc = rr*ciso**2
#else
                 ploc = pl
                 proc = pr
#endif
                 
                 ! left variables
                 call left_var!(ul, vl, wl, rl, bl, cl, entho, a, ploc, ecinl, emagl, etotl, ptotl, vdotbl)
                 !ecinl  = half*(ul*ul + vl*vl + wl*wl)*rl
                 !emagl  = half*(a*a + bl*bl + cl*cl)
                 !etotl  = ploc*entho + ecinl + emagl
                 !ptotl  = ploc + emagl
                 !vdotbl = ul*a + vl*bl + wl*cl
                 
                 ! right variables
                 ecinr  = half*(ur*ur + vr*vr + wr*wr)*rr
                 emagr  = half*(a*a + br*br + cr*cr)
                 etotr  = proc*entho + ecinr + emagr
                 ptotr  = proc + emagr
                 vdotbr = ur*a + vr*br + wr*cr
               
                 c2     = gamma*ploc/rl
                 b2     = a*a + bl*bl + cl*cl
                 d2     = half*(c2 + b2/rl)
                 cfastl = sqrt(d2 + sqrt(d2**2 - c2*a*a/rl))
               
                 c2     = gamma*proc/rr
                 b2     = a*a + br*br + cr*cr
                 d2     = half*(c2 + b2/rr)
                 cfastr = sqrt(d2 + sqrt(d2**2 - c2*a*a/rr))
               
                 ! compute hll wave speed
                 sl = min(ul, ur) - max(cfastl, cfastr)
                 sr = max(ul, ur) + max(cfastl, cfastr)
                 
                 ! compute lagrangian sound speed
                 rcl = rl*(ul - sl)
                 rcr = rr*(sr - ur)
                 
                 ! compute acoustic star state
                 ustar    = (rcr*ur + rcl*ul + (ptotl - ptotr))/(rcr + rcl)
                 ptotstar = (rcr*ptotl + rcl*ptotr + rcl*rcr*(ul-ur))/(rcr + rcl)
                 
                 ! left star region variables
                 rstarl = rl*(sl - ul)/(sl - ustar)
                 estar  = rl*(sl - ul)*(sl - ustar) - a**2
                 el     = rl*(sl - ul)*(sl - ul) - a**2
                 if (estar == zero) then
                    vstarl = vl
                    bstarl = bl
                    wstarl = wl
                    cstarl = cl
                 else
                    vstarl = vl - a*bl*(ustar - ul)/estar
                    bstarl = bl*el/estar
                    wstarl = wl - a*cl*(ustar - ul)/estar
                    cstarl = cl*el/estar
                 endif
                 vdotbstarl = ustar*a + vstarl*bstarl + wstarl*cstarl
                 etotstarl  = ((sl - ul)*etotl - ptotl*ul + ptotstar*ustar &
                            + a*(vdotbl - vdotbstarl))/(sl - ustar)
                 sqrrstarl  = sqrt(rstarl)
                 calfvenl   = abs(a)/sqrrstarl
                 sal        = ustar - calfvenl
                 
                 ! right star region variables
                 rstarr = rr*(sr - ur)/(sr - ustar)
                 estar  = rr*(sr - ur)*(sr - ustar) - a**2
                 er     = rr*(sr - ur)*(sr - ur) - a**2
                 if (estar == zero) then
                    vstarr = vr
                    bstarr = br
                    wstarr = wr
                    cstarr = cr
                 else
                    vstarr = vr - a*br*(ustar - ur)/estar
                    bstarr = br*er/estar
                    wstarr = wr - a*cr*(ustar - ur)/estar
                    cstarr = cr*er/estar
                 endif
                 vdotbstarr = ustar*a + vstarr*bstarr + wstarr*cstarr
                 etotstarr  = ((sr - ur)*etotr - ptotr*ur + ptotstar*ustar &
                            + a*(vdotbr - vdotbstarr))/(sr - ustar)
                 sqrrstarr  = sqrt(rstarr)
                 calfvenr   = abs(a)/sqrrstarr
                 sar        = ustar + calfvenr
                 
                 ! double star region variables
                 vstarstar = (sqrrstarl*vstarl + sqrrstarr*vstarr &
                         + sgnm*(bstarr - bstarl))/(sqrrstarl + sqrrstarr)
                 wstarstar = (sqrrstarl*wstarl + sqrrstarr*wstarr &
                         + sgnm*(cstarr - cstarl))/(sqrrstarl + sqrrstarr)
                 bstarstar = (sqrrstarl*bstarr + sqrrstarr*bstarl &
                         + sgnm*sqrrstarl*sqrrstarr*(vstarr - vstarl))/(sqrrstarl + sqrrstarr)
                 cstarstar = (sqrrstarl*cstarr + sqrrstarr*cstarl &
                         + sgnm*sqrrstarl*sqrrstarr*(wstarr - wstarl))/(sqrrstarl + sqrrstarr)
                 vdotbstarstar = ustar*a + vstarstar*bstarstar + wstarstar*cstarstar
                 etotstarstarl = etotstarl - sgnm*sqrrstarl*(vdotbstarl - vdotbstarstar)
                 etotstarstarr = etotstarr + sgnm*sqrrstarr*(vdotbstarr - vdotbstarstar)
                 
                 ! sample the solution at x/t=0
                 if (sl > zero) then
                    ro = rl
                    uo = ul
                    vo = vl
                    wo = wl
                    bo = bl
                    co = cl
                    ptoto  = ptotl
                    etoto  = etotl
                    vdotbo = vdotbl
                 else if (sal > zero) then
                    ro = rstarl
                    uo = ustar
                    vo = vstarl
                    wo = wstarl
                    bo = bstarl
                    co = cstarl
                    ptoto  = ptotstar
                    etoto  = etotstarl
                    vdotbo = vdotbstarl
                 else if (ustar > zero) then
                    ro = rstarl
                    uo = ustar
                    vo = vstarstar
                    wo = wstarstar
                    bo = bstarstar
                    co = cstarstar
                    ptoto  = ptotstar
                    etoto  = etotstarstarl
                    vdotbo = vdotbstarstar
                 else if (sar > zero)then
                    ro = rstarr
                    uo = ustar
                    vo = vstarstar
                    wo = wstarstar
                    bo = bstarstar
                    co = cstarstar
                    ptoto  = ptotstar
                    etoto  = etotstarstarr
                    vdotbo = vdotbstarstar
                 else if (sr > zero)then
                    ro = rstarr
                    uo = ustar
                    vo = vstarr
                    wo = wstarr
                    bo = bstarr
                    co = cstarr
                    ptoto  = ptotstar
                    etoto  = etotstarr
                    vdotbo = vdotbstarr
                 else
                    ro = rr
                    uo = ur
                    vo = vr
                    wo = wr
                    bo = br
                    co = cr
                    ptoto  = ptotr
                    etoto  = etotr
                    vdotbo = vdotbr
                 end if
                 
                 ! compute the godunov flux
                 fgodunov(i,j,k,1,idim) = ro*uo
                 fgodunov(i,j,k,2,idim) = (etoto + ptoto)*uo - a*vdotbo
                 fgodunov(i,j,k,3,idim) = ro*uo*uo - a*a
                 fgodunov(i,j,k,4,idim) = zero
                 fgodunov(i,j,k,5,idim) = ro*uo*vo - a*bo
                 fgodunov(i,j,k,6,idim) = bo*uo - a*vo
                 fgodunov(i,j,k,7,idim) = ro*uo*wo - a*co
                 fgodunov(i,j,k,8,idim) = co*uo - a*wo
                 
                 rgstar(i,j,k) = ro
                 ugstar(i,j,k) = uo
                 vgstar(i,j,k) = vo
                 wgstar(i,j,k) = wo
                 bgstar(i,j,k) = bo
                 cgstar(i,j,k) = co
                 pgstar(i,j,k) = ptoto
              end if

              ! Upwind solver in case of the shearing box (only on the hydro
              ! variables))
#if GEOM == CARTESIAN
#if NDIM == 1
              if (Omega0 > zero) then
                 shear = -1.5d0*Omega0*(x(i) + x(i-1))
                 fgodunov(i,j,k,8,idim) = fgodunov(i,j,k,8,idim) &
                                      & + shear*bn_mean
              endif
#endif
#if NDIM == 2
              if (Omega0 > zero) then
                 shear = -1.5d0*Omega0*x(i)
                 if (idim == 1) shear = -1.5d0*Omega0*(x(i) + x(i-1))
                 if (idim == 2) shear = -1.5d0*Omega0*x(i)
                 fgodunov(i,j,k,8,idim) = fgodunov(i,j,k,8,idim) &
                                      & + shear*bn_mean
              endif
#endif
#if NDIM == 3
              if ((Omega0 > zero) .and. (idim == 2) .and. (.not. fargo)) then
                 shear = -1.5d0*Omega0*x(i)
                 if (shear > zero) then
                    Emag = half*(al**2 + bl**2 + cl**2)
                    Ekin = half*(ul**2 + vl**2 + wl**2)
                    Etot = Ekin + Emag + pl/(gamma - one)
                    fgodunov(i,j,k,1,idim) = fgodunov(i,j,k,1,idim) &
                            & + shear*rl
                    fgodunov(i,j,k,2,idim) = fgodunov(i,j,k,2,idim) &
                            & + shear*(Etot + Emag - bn_mean**2)
                    fgodunov(i,j,k,3,idim) = fgodunov(i,j,k,3,idim) &
                            & + shear*rl*ul
                    fgodunov(i,j,k,5,idim) = fgodunov(i,j,k,5,idim) &
                            & + shear*rl*vl
                    fgodunov(i,j,k,7,idim) = fgodunov(i,j,k,7,idim) &
                            & + shear*rl*wl
                 else
                    Emag = half*(ar**2 + br**2 + cr**2)
                    Ekin = half*(ur**2 + vr**2 + wr**2)
                    Etot = Ekin + Emag + pr/(gamma - one)
                    fgodunov(i,j,k,1,idim) = fgodunov(i,j,k,1,idim) &
                            & + shear*rr
                    fgodunov(i,j,k,2,idim) = fgodunov(i,j,k,2,idim) &
                            & + shear*(Etot + Emag - bn_mean**2)
                    fgodunov(i,j,k,3,idim) = fgodunov(i,j,k,3,idim) &
                            & + shear*rr*ur
                    fgodunov(i,j,k,5,idim) = fgodunov(i,j,k,5,idim) &
                            & + shear*rr*vr
                    fgodunov(i,j,k,7,idim) = fgodunov(i,j,k,7,idim) &
                            & + shear*rr*wr
                 endif
              endif
#endif
#endif
           enddo
        enddo
     enddo
     !$OMP END PARALLEL DO

     ! Add geometrical terms
#if GEOM == CARTESIAN
     !$acc kernels loop
     !$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(ii, ji, ki)
     do k = klo, khi - koffset
        do j = jlo, jhi - joffset
           do i = ilo, ihi - ioffset
              ii = i + ioffset; ji = j + joffset; ki = k + koffset
              fgodunov_pre(i,j,k,idim) = -(pgstar(ii,ji,ki)*ds(ii,ji,ki,idim) &
                                         - pgstar(i,j,k)*ds(i,j,k,idim))*dt
           enddo
        enddo
     enddo
     !$OMP END PARALLEL DO
#endif

#if GEOM == CYLINDRICAL
     if (idim == 1) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(ro, vo, bo, pressure)
        do k = klo, khi - koffset
           do j = jlo, jhi - joffset
              do i = ilo, ihi - ioffset
                 fgodunov_pre(i,j,k,1) = -dv(i,j,k)*(pgstar(i+1,j,k) &
                                                 & - pgstar(i,j,k))/dx
                 ro = half*(rgstar(i,j,k) + rgstar(i+1,j,k))
                 vo = half*(vgstar(i,j,k) + vgstar(i+1,j,k))
                 bo = half*(bgstar(i,j,k) + bgstar(i+1,j,k))
                 ! pressure = (ro*vo*vo - bo*bo)*(ds(i+1,j,k,1) - ds(i,j,k,1))
                 pressure = -bo*bo*(ds(i+1,j,k,1) - ds(i,j,k,1))
                 fgodunov_pre(i,j,k,1) = fgodunov_pre(i,j,k,1) + pressure
                 fgodunov_pre(i,j,k,1) = fgodunov_pre(i,j,k,1)*dt
              enddo
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif

     if ((idim == 2).or.(idim == 3)) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(ii, ji, ki)
        do k = klo, khi - koffset
           do j = jlo, jhi - joffset
              do i = ilo, ihi - ioffset
                 ii = i + ioffset; ji = j + joffset; ki = k + koffset
                 fgodunov_pre(i,j,k,idim) = -(pgstar(ii,ji,ki)*ds(ii,ji,ki,idim)&
                                            - pgstar(i,j,k)*ds(i,j,k,idim))*dt
              enddo
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif

     ! Account for initertial frame
     !$acc kernels loop
     !$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(xL)
     do k = klo , khi
        do j = jlo , jhi
           do i = ilo, ihi
              xL = half*(x(i) + x(i-1))
              if (idim == 1) fgodunov(i,j,k,5,1) = fgodunov(i,j,k,5,1) &
                   & + rgstar(i,j,k)*ugstar(i,j,k)*Omega0*xL
              if (idim == 2) fgodunov(i,j,k,3,2)  = fgodunov(i,j,k,3,2) &
                   & + rgstar(i,j,k)*ugstar(i,j,k)*Omega0*x(i)
              if (idim == 3) fgodunov(i,j,k,7,3) = fgodunov(i,j,k,7,3) &
                   & + rgstar(i,j,k)*ugstar(i,j,k)*Omega0*x(i)
           enddo
        enddo
     enddo
     !$OMP END PARALLEL DO
#endif

#if GEOM == SPHERICAL
     if (idim == 1) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(ro, vo, wo, bo, co, pressure)
        do k = klo, khi - koffset
           do j = jlo, jhi - joffset
              do i = ilo, ihi - ioffset
                 fgodunov_pre(i,j,k,1) = -dv(i,j,k)*(pgstar(i+1,j,k) &
                                                 & - pgstar(i,j,k))/dx
                 ro = half*(rgstar(i,j,k) + rgstar(i+1,j,k))
                 vo = half*(vgstar(i,j,k) + vgstar(i+1,j,k))
                 wo = half*(wgstar(i,j,k) + wgstar(i+1,j,k))
                 bo = half*(bgstar(i,j,k) + bgstar(i+1,j,k))
                 co = half*(cgstar(i,j,k) + cgstar(i+1,j,k))
                 pressure = (ro*(vo*vo + wo*wo) - bo*bo - co*co)*(ds(i+1,j,k,1)&
                                                              & - ds(i,j,k,1))
                 fgodunov_pre(i,j,k,1) = fgodunov_pre(i,j,k,1) + pressure
                 fgodunov_pre(i,j,k,1) = fgodunov_pre(i,j,k,1)*dt
              enddo
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif

     if (idim == 2) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(cotanxc, ro, wo, co, pressure)
        do k = klo, khi - koffset
           do j = jlo, jhi - joffset
              cotanxc = cos(y(j))/sin(y(j))
              do i = ilo, ihi - ioffset
                 fgodunov_pre(i,j,k,idim) = -dv(i,j,k)*(pgstar(i,j+1,k) &
                                                 & - pgstar(i,j,k))/dx
                 ro = half*(rgstar(i,j,k) + rgstar(i,j+1,k))
                 wo = half*(wgstar(i,j,k) + wgstar(i,j+1,k))
                 co = half*(cgstar(i,j,k) + cgstar(i,j+1,k))
                 pressure = (ro*wo*wo - co*co)*cotanxc*half*(ds(i,j+1,k,1) &
                                                         & - ds(i,j,k,1))
                 fgodunov_pre(i,j,k,idim) = fgodunov_pre(i,j,k,idim) + pressure
                 fgodunov_pre(i,j,k,idim) = fgodunov_pre(i,j,k,idim)*dt
              enddo
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif

     if (idim == 3) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(ii, ji, ki)
        do k = klo, khi - koffset
           do j = jlo, jhi - joffset
              do i = ilo, ihi - ioffset
                 ii = i + ioffset; ji = j + joffset; ki = k + koffset
                 fgodunov_pre(i,j,k,idim) = -(pgstar(ii,ji,ki)*ds(ii,ji,ki,idim)&
                                            - pgstar(i,j,k)*ds(i,j,k,idim))*dt
              enddo
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif
#endif
  enddo

  !$acc end data

  deallocate(rgstar, ugstar, vgstar, wgstar, bgstar, cgstar, pgstar)

    return
  end subroutine riemann_solver

  !subroutine hlld(qm, qp, fgodunov, rgstar, ugstar, vgstar, wgstar, bgstar, cgstar, pgstar, ro, uo, vo, wo, bo, co, ptoto, rl, pl, ul, vl, wl, cl, bl, al, rr, pr, ur, vr, wr, cr, br, ar)
  !  !$acc routine
  !  implicit none
  !
  !  real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2,nvar,ndim), intent(in) :: qm 
  !  real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2,nvar,ndim), intent(in) :: qp 
  !  real(dp), intent(inout) :: rl, pl, ul, vl, wl, cl, bl, al
  !  real(dp), intent(inout) :: rr, pr, ur, vr, wr, cr, br, ar
  !  real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2,nvar,ndim),intent(out) :: fgodunov
  !  real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2), intent(inout) :: rgstar, ugstar, vgstar, wgstar
  !  real(dp), dimension(iu1:iu2,ju1:ju2,ku1:ku2), intent(inout) :: bgstar, cgstar, pgstar
  !
  !  real(dp), intent(inout) :: ro, uo, vo, wo, bo, co, ptoto
  !
  !real(dp) :: sl, sr, sal, sar
  !real(dp) :: entho, a, sgnm, ploc, proc
  !real(dp) :: ecinl, emagl, etotl, ptotl, vdotbl, el, cfastl, calfvenl, rcl
  !real(dp) :: ecinr, emagr, etotr, ptotr, vdotbr, er, cfastr, calfvenr, rcr
  !real(dp) :: rstarl, vstarl, wstarl, bstarl, cstarl, vdotbstarl
  !real(dp) :: rstarr, vstarr, wstarr, bstarr, cstarr, vdotbstarr
  !real(dp) :: sqrrstarl, etotstarl, etotstarstarl
  !real(dp) :: sqrrstarr, etotstarr, etotstarstarr
  !real(dp) :: ustar, ptotstar, estar, vstarstar, wstarstar, bstarstar
  !real(dp) :: cstarstar, vdotbstarstar
  !real(dp) :: etoto, vdotbo
  !real(dp) :: c2, b2, d2, cf
  !
  !integer :: i, j, k, idim
  !
  !entho = one/(gamma-one)
  !
  !! enforce continuity of normal component
  !a    = half*(al + ar)
  !sgnm = sign(one, a)
  !al   = a; ar = a
  !
#i!f ISO == 1
  !ploc = rl*ciso**2
  !proc = rr*ciso**2
#e!lse
  !ploc = pl
  !proc = pr
#e!ndif
  !
  !! left variables
  !ecinl  = half*(ul*ul + vl*vl + wl*wl)*rl
  !emagl  = half*(a*a + bl*bl + cl*cl)
  !etotl  = ploc*entho + ecinl + emagl
  !ptotl  = ploc + emagl
  !vdotbl = ul*a + vl*bl + wl*cl
  !
  !! right variables
  !ecinr  = half*(ur*ur + vr*vr + wr*wr)*rr
  !emagr  = half*(a*a + br*br + cr*cr)
  !etotr  = proc*entho + ecinr + emagr
  !ptotr  = proc + emagr
  !vdotbr = ur*a + vr*br + wr*cr
  !
  !c2     = gamma*ploc/rl
  !b2     = a*a + bl*bl + cl*cl
  !d2     = half*(c2 + b2/rl)
  !cfastl = sqrt(d2 + sqrt(d2**2 - c2*a*a/rl))
  !
  !c2     = gamma*proc/rr
  !b2     = a*a + br*br + cr*cr
  !d2     = half*(c2 + b2/rr)
  !cfastr = sqrt(d2 + sqrt(d2**2 - c2*a*a/rr))
  !
  !! compute hll wave speed
  !sl = min(ul, ur) - max(cfastl, cfastr)
  !sr = max(ul, ur) + max(cfastl, cfastr)
  !
  !! compute lagrangian sound speed
  !rcl = rl*(ul - sl)
  !rcr = rr*(sr - ur)
  !
  !! compute acoustic star state
  !ustar    = (rcr*ur + rcl*ul + (ptotl - ptotr))/(rcr + rcl)
  !ptotstar = (rcr*ptotl + rcl*ptotr + rcl*rcr*(ul-ur))/(rcr + rcl)
  !
  !! left star region variables
  !rstarl = rl*(sl - ul)/(sl - ustar)
  !estar  = rl*(sl - ul)*(sl - ustar) - a**2
  !el     = rl*(sl - ul)*(sl - ul) - a**2
  !if (estar == zero) then
  !   vstarl = vl
  !   bstarl = bl
  !   wstarl = wl
  !   cstarl = cl
  !else
  !   vstarl = vl - a*bl*(ustar - ul)/estar
  !   bstarl = bl*el/estar
  !   wstarl = wl - a*cl*(ustar - ul)/estar
  !   cstarl = cl*el/estar
  !endif
  !vdotbstarl = ustar*a + vstarl*bstarl + wstarl*cstarl
  !etotstarl  = ((sl - ul)*etotl - ptotl*ul + ptotstar*ustar &
  !           + a*(vdotbl - vdotbstarl))/(sl - ustar)
  !sqrrstarl  = sqrt(rstarl)
  !calfvenl   = abs(a)/sqrrstarl
  !sal        = ustar - calfvenl
  !
  !! right star region variables
  !rstarr = rr*(sr - ur)/(sr - ustar)
  !estar  = rr*(sr - ur)*(sr - ustar) - a**2
  !er     = rr*(sr - ur)*(sr - ur) - a**2
  !if (estar == zero) then
  !   vstarr = vr
  !   bstarr = br
  !   wstarr = wr
  !   cstarr = cr
  !else
  !   vstarr = vr - a*br*(ustar - ur)/estar
  !   bstarr = br*er/estar
  !   wstarr = wr - a*cr*(ustar - ur)/estar
  !   cstarr = cr*er/estar
  !endif
  !vdotbstarr = ustar*a + vstarr*bstarr + wstarr*cstarr
  !etotstarr  = ((sr - ur)*etotr - ptotr*ur + ptotstar*ustar &
  !           + a*(vdotbr - vdotbstarr))/(sr - ustar)
  !sqrrstarr  = sqrt(rstarr)
  !calfvenr   = abs(a)/sqrrstarr
  !sar        = ustar + calfvenr
  !
  !! double star region variables
  !vstarstar = (sqrrstarl*vstarl + sqrrstarr*vstarr &
  !        + sgnm*(bstarr - bstarl))/(sqrrstarl + sqrrstarr)
  !wstarstar = (sqrrstarl*wstarl + sqrrstarr*wstarr &
  !        + sgnm*(cstarr - cstarl))/(sqrrstarl + sqrrstarr)
  !bstarstar = (sqrrstarl*bstarr + sqrrstarr*bstarl &
  !        + sgnm*sqrrstarl*sqrrstarr*(vstarr - vstarl))/(sqrrstarl + sqrrstarr)
  !cstarstar = (sqrrstarl*cstarr + sqrrstarr*cstarl &
  !        + sgnm*sqrrstarl*sqrrstarr*(wstarr - wstarl))/(sqrrstarl + sqrrstarr)
  !vdotbstarstar = ustar*a + vstarstar*bstarstar + wstarstar*cstarstar
  !etotstarstarl = etotstarl - sgnm*sqrrstarl*(vdotbstarl - vdotbstarstar)
  !etotstarstarr = etotstarr + sgnm*sqrrstarr*(vdotbstarr - vdotbstarstar)
  !
  !! sample the solution at x/t=0
  !if (sl > zero) then
  !   ro = rl
  !   uo = ul
  !   vo = vl
  !   wo = wl
  !   bo = bl
  !   co = cl
  !   ptoto  = ptotl
  !   etoto  = etotl
  !   vdotbo = vdotbl
  !else if (sal > zero) then
  !   ro = rstarl
  !   uo = ustar
  !   vo = vstarl
  !   wo = wstarl
  !   bo = bstarl
  !   co = cstarl
  !   ptoto  = ptotstar
  !   etoto  = etotstarl
  !   vdotbo = vdotbstarl
  !else if (ustar > zero) then
  !   ro = rstarl
  !   uo = ustar
  !   vo = vstarstar
  !   wo = wstarstar
  !   bo = bstarstar
  !   co = cstarstar
  !   ptoto  = ptotstar
  !   etoto  = etotstarstarl
  !   vdotbo = vdotbstarstar
  !else if (sar > zero)then
  !   ro = rstarr
  !   uo = ustar
  !   vo = vstarstar
  !   wo = wstarstar
  !   bo = bstarstar
  !   co = cstarstar
  !   ptoto  = ptotstar
  !   etoto  = etotstarstarr
  !   vdotbo = vdotbstarstar
  !else if (sr > zero)then
  !   ro = rstarr
  !   uo = ustar
  !   vo = vstarr
  !   wo = wstarr
  !   bo = bstarr
  !   co = cstarr
  !   ptoto  = ptotstar
  !   etoto  = etotstarr
  !   vdotbo = vdotbstarr
  !else
  !   ro = rr
  !   uo = ur
  !   vo = vr
  !   wo = wr
  !   bo = br
  !   co = cr
  !   ptoto  = ptotr
  !   etoto  = etotr
  !   vdotbo = vdotbr
  !end if
  !
  !! compute the godunov flux
  !fgodunov(i,j,k,1,idim) = ro*uo
  !fgodunov(i,j,k,2,idim) = (etoto + ptoto)*uo - a*vdotbo
  !fgodunov(i,j,k,3,idim) = ro*uo*uo - a*a
  !fgodunov(i,j,k,4,idim) = zero
  !fgodunov(i,j,k,5,idim) = ro*uo*vo - a*bo
  !fgodunov(i,j,k,6,idim) = bo*uo - a*vo
  !fgodunov(i,j,k,7,idim) = ro*uo*wo - a*co
  !fgodunov(i,j,k,8,idim) = co*uo - a*wo
  !
  !rgstar(i,j,k) = ro
  !ugstar(i,j,k) = uo
  !vgstar(i,j,k) = vo
  !wgstar(i,j,k) = wo
  !bgstar(i,j,k) = bo
  !cgstar(i,j,k) = co
  !pgstar(i,j,k) = ptoto
  !
  !
  !  return
  !end subroutine hlld

  subroutine left_var!(ul, vl, wl, rl, bl, cl, entho, a, ploc, ecinl, emagl, etotl, ptotl, vdotbl)
    !$acc routine
    implicit none

    !real(dp), intent(in) :: rl, ul, vl, wl, cl, bl, entho, a, ploc
    !real(dp), intent(out) :: ecinl, emagl, etotl, ptotl, vdotbl

    ! left variables
    ecinl  = half*(ul*ul + vl*vl + wl*wl)*rl
    emagl  = half*(a*a + bl*bl + cl*cl)
    etotl  = ploc*entho + ecinl + emagl
    ptotl  = ploc + emagl
    vdotbl = ul*a + vl*bl + wl*cl
    
    return
  end subroutine left_var

end module solver
