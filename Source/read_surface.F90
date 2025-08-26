!!! *** Copyright Notice ***
!!! “CrunchFlow”, Copyright (c) 2016, The Regents of the University of California, through Lawrence Berkeley National Laboratory 
!!! (subject to receipt of any required approvals from the U.S. Dept. of Energy).  All rights reserved.
!!! 
!!! If you have questions about your rights to use or distribute this software, please contact 
!!! Berkeley Lab's Innovation & Partnerships Office at  IPO@lbl.gov.
!!! 
!!! NOTICE.  This Software was developed under funding from the U.S. Department of Energy and the U.S. Government 
!!! consequently retains certain rights. As such, the U.S. Government has been granted for itself and others acting 
!!! on its behalf a paid-up, nonexclusive, irrevocable, worldwide license in the Software to reproduce, distribute copies to the public, 
!!! prepare derivative works, and perform publicly and display publicly, and to permit other to do so.
!!!
!!! *** License Agreement ***
!!! “CrunchFlow”, Copyright (c) 2016, The Regents of the University of California, through Lawrence Berkeley National Laboratory)
!!! subject to receipt of any required approvals from the U.S. Dept. of Energy).  All rights reserved."
!!! 
!!! Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
!!! 
!!! (1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
!!!
!!! (2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer 
!!! in the documentation and/or other materials provided with the distribution.
!!!
!!! (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory, U.S. Dept. of Energy nor the names of 
!!! its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
!!!
!!! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, 
!!! BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
!!! SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
!!! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
!!! OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
!!! LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
!!! THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!!!
!!! You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the features, functionality or 
!!! performance of the source code ("Enhancements") to anyone; however, if you choose to make your
!!! Enhancements available either publicly, or directly to Lawrence Berkeley National Laboratory, without 
!!! imposing a separate written license agreement for such 
!!! Enhancements, then you hereby grant the following license: a  non-exclusive, royalty-free perpetual license to install, use, 
!!! modify, prepare derivative works, incorporate into other computer software, distribute, and sublicense such enhancements or 
!!! derivative works thereof, in binary and source code form.

!!!      ****************************************
    
SUBROUTINE read_surface(nout,ncomp,nkin,nsurf)
USE crunchtype
USE params
USE concentration
USE mineral
USE strings

IMPLICIT NONE

!  External variables and arrays

INTEGER(I4B), INTENT(IN)                                    :: nout
INTEGER(I4B), INTENT(IN)                                    :: ncomp
INTEGER(I4B), INTENT(IN)                                    :: nkin
INTEGER(I4B), INTENT(OUT)                                   :: nsurf

!  Internal variables and arrays

CHARACTER (LEN=mls)                                         :: namtmp
CHARACTER (LEN=mls)                                         :: namsurfsave

INTEGER(I4B)                                                :: id
INTEGER(I4B)                                                :: iff
INTEGER(I4B)                                                :: ids
INTEGER(I4B)                                                :: ls
INTEGER(I4B)                                                :: lzs
INTEGER(I4B)                                                :: lsurf
INTEGER(I4B)                                                :: lmin
INTEGER(I4B)                                                :: k

REWIND nout

nsurf = 0

100 READ(nout,'(a)',END=111) zone
id = 1
iff = mls
CALL sschaine(zone,id,iff,ssch,ids,ls)
IF(ls /= 0) THEN
  IF (ssch(1:1) == '>') THEN
    lsurf = ls
    nsurf = nsurf + 1
    IF (nsurf > msurf) THEN
      WRITE(*,*)
      WRITE(*,*) ' Number of surface sites dimensioned too small'
      WRITE(*,*) ' Msurf = ',msurf
      WRITE(*,*)
      READ(*,*)
      STOP
    END IF
    namsurf(nsurf) = ssch
  ELSE
    GO TO 100
  END IF
  namsurfsave = namsurf(nsurf)

  id = ids + ls
  CALL sschaine(zone,id,iff,ssch,ids,ls)
  IF (ls /= 0) THEN
    lzs=ls
    CALL convan(ssch,lzs,res)
    IF (ssch /= 'on') THEN
      WRITE(*,*) ' Surface complex should be followed by "on" '
      WRITE(*,*) ' Surface complex: ',namsurfsave(1:lsurf)
      WRITE(*,*)
      READ(*,*)
      STOP
    END IF
    id = ids + ls
    CALL sschaine(zone,id,iff,ssch,ids,ls)
    lzs=ls
    lmin = ls
    CALL stringtype(ssch,lzs,res)
    IF (res /= 'a') THEN
      WRITE(*,*)
      WRITE(*,*) ' Looking for a mineral name, not a number'
      WRITE(*,*) ' In surface complexation block'
      WRITE(*,*) ' Surface complex: ',namsurfsave(1:lsurf)
      WRITE(*,*)
      READ(*,*)
      STOP
    END IF
    namtmp = ssch
    DO k = 1,nkin
      IF (namtmp == umin(k)) THEN
        ksurf(nsurf) = k
        GO TO 200
      END IF
    END DO
    WRITE(*,*)
    WRITE(*,*) ' Mineral substrate listed in surface complexation block'
    WRITE(*,*) '     not found in minerals list'
    WRITE(*,*) ' Surface complex: ',namsurfsave(1:lsurf)
    WRITE(*,*) ' Looking for mineral: ',namtmp(1:lmin)
    WRITE(*,*)
    READ(*,*)
    STOP
    
    200    CONTINUE
    
  ELSE
    WRITE(*,*)
    WRITE(*,*) ' Mineral host for surface hydroxyl site must be specified'
    WRITE(*,*) ' In surface complexation block'
    WRITE(*,*) ' Surface complex: ',namsurfsave(1:lsurf)
    WRITE(*,*)
    READ(*,*)
    STOP
  END IF

  id = ids + ls
  CALL sschaine(zone,id,iff,ssch,ids,ls)
  IF (ls /= 0) THEN
    lzs=ls
    CALL convan(ssch,lzs,res)
    IF (ssch == '-no_edl' .OR. ssch == '--no_edl') THEN
      iedl(nsurf) = 1
    END IF
  ELSE
    GO TO 100
  END IF
  
END IF

GO TO 100

111  CONTINUE

RETURN

END SUBROUTINE read_surface

!*********************************************************************
! New parser for optional electrical double layer (EDL) parameters
! The block is expected to appear in the database as
!   Begin edl parameters
!   <site>  C1  C2  eps_r
!   ...
!   End edl parameters
! The routine maps each entry to an existing surface site and stores the
! capacitance values and dielectric constant.
!*********************************************************************
SUBROUTINE read_edl_parameters(iunit,nsurf,nsurf_sec)
  USE crunchtype
  USE params
  USE concentration
  USE strings
  USE CrunchFunctions

  IMPLICIT NONE

  INTEGER(I4B), INTENT(IN) :: iunit
  INTEGER(I4B), INTENT(IN) :: nsurf
  INTEGER(I4B), INTENT(IN) :: nsurf_sec

  CHARACTER (LEN=mls) :: line
  CHARACTER (LEN=mls) :: name
  INTEGER(I4B) :: ifind
  INTEGER(I4B) :: id, iff, ids, ls
  INTEGER(I4B) :: is, ns, idx
  REAL(DP) :: val1, val2, val3

  ! Initialize in case block is absent
  IF (.NOT. ALLOCATED(C1)) RETURN
  C1 = 0.0d0
  C2 = 0.0d0
  eps_r = 0.0d0

  REWIND iunit
  line = 'Begin edl parameters'
  CALL find_string(iunit,line,ifind)
  IF (ifind == 0) THEN
    REWIND iunit
    RETURN
  END IF

  DO
    READ(iunit,'(a)',END=100) line
    IF (line == 'End edl parameters') EXIT
    id = 1
    iff = mls
    CALL sschaine(line,id,iff,ssch,ids,ls)
    IF (ls == 0) CYCLE
    name = ssch
    idx = 0
    DO is = 1,nsurf
      IF (name == namsurf(is)) THEN
        idx = is
        EXIT
      END IF
    END DO
    IF (idx == 0) THEN
      DO ns = 1,nsurf_sec
        IF (name == namsurf_sec(ns)) THEN
          idx = ns + nsurf
          EXIT
        END IF
      END DO
    END IF
    IF (idx == 0) CYCLE

    id = ids + ls
    CALL sschaine(line,id,iff,ssch,ids,ls)
    IF (ls /= 0) THEN
      val1 = DNUM(ssch)
      id = ids + ls
      CALL sschaine(line,id,iff,ssch,ids,ls)
      IF (ls /= 0) THEN
        val2 = DNUM(ssch)
        id = ids + ls
        CALL sschaine(line,id,iff,ssch,ids,ls)
        IF (ls /= 0) THEN
          val3 = DNUM(ssch)
          C1(idx) = val1
          C2(idx) = val2
          eps_r(idx) = val3
        END IF
      END IF
    END IF
  END DO

100 CONTINUE
  REWIND iunit

END SUBROUTINE read_edl_parameters
