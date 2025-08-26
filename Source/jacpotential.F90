!!! *** Copyright Notice ***
!!! CrunchFlow, Copyright (c) 2016, The Regents of the University of California,
!!! through Lawrence Berkeley National Laboratory. All rights reserved.
!!!
!!! Redistribution and use in source and binary forms, with or without
!!! modification, are permitted provided that the following conditions are met:
!!! (1) Redistributions of source code must retain the above copyright notice,
!!!     this list of conditions and the following disclaimer.
!!! (2) Redistributions in binary form must reproduce the above copyright notice,
!!!     this list of conditions and the following disclaimer in the documentation
!!!     and/or other materials provided with the distribution.
!!! (3) Neither the name of the University of California, Lawrence Berkeley
!!!     National Laboratory, U.S. Dept. of Energy nor the names of its
!!!     contributors may be used to endorse or promote products derived from
!!!     this software without specific prior written permission.
!!!
!!! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
!!! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!!! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
!!! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
!!! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!!! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!!! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!!! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!!! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!!! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!!! POSSIBILITY OF SUCH DAMAGE.

SUBROUTINE jacpotential(ncomp,nsurf,nsurf_sec,npot,nx,ny,nz)
USE crunchtype
USE concentration
USE solver
USE mineral

IMPLICIT NONE

!  External variables and arrays
INTEGER(I4B), INTENT(IN) :: ncomp
INTEGER(I4B), INTENT(IN) :: nsurf
INTEGER(I4B), INTENT(IN) :: nsurf_sec
INTEGER(I4B), INTENT(IN) :: npot
INTEGER(I4B), INTENT(IN) :: nx
INTEGER(I4B), INTENT(IN) :: ny
INTEGER(I4B), INTENT(IN) :: nz

!  Internal variables and arrays
INTEGER(I4B) :: jx, jy, jz
INTEGER(I4B) :: npt2, i, is, ns
INTEGER(I4B) :: idxR0, idxRb, idxRd, k
REAL(DP)    :: surfconc, correct, term1
REAL(DP)    :: sum0, sumb

fjpotncomp = 0.0d0
fjpotnsurf = 0.0d0

DO jz = 1,nz
  DO jy = 1,ny
    DO jx = 1,nx

      DO npt2 = 1,npot

        k = ksurf(ispot(npt2))
        correct = wtmin(k)*specificByGrid(k,jx,jy,jz)*volfx(k,jx,jy,jz)/volmol(k)
        term1 = 96485.0d0/correct

        idxR0 = npt2
        idxRb = npt2 + npot
        idxRd = npt2 + 2*npot

        DO i = 1,ncomp
          sum0 = 0.0d0
          sumb = 0.0d0
          DO ns = 1,nsurf_sec
            IF (ksurf(islink(ns)) == kpot(npt2)) THEN
              surfconc = spsurf10(ns+nsurf,jx,jy,jz)
              sum0 = sum0 - musurf(ns,i)*z0_s(ns+nsurf)*surfconc*term1
              sumb = sumb - musurf(ns,i)*zb_s(ns+nsurf)*surfconc*term1
            END IF
          END DO
          fjpotncomp(idxR0,i,jx,jy,jz) = sum0
          fjpotncomp(idxRb,i,jx,jy,jz) = sumb
          fjpotncomp(idxRd,i,jx,jy,jz) = 0.0d0
        END DO

        DO is = 1,nsurf
          sum0 = 0.0d0
          sumb = 0.0d0
          DO ns = 1,nsurf_sec
            IF (ksurf(islink(ns)) == kpot(npt2)) THEN
              surfconc = spsurf10(ns+nsurf,jx,jy,jz)
              sum0 = sum0 - musurf(ns,is+ncomp)*z0_s(ns+nsurf)*surfconc*term1
              sumb = sumb - musurf(ns,is+ncomp)*zb_s(ns+nsurf)*surfconc*term1
            END IF
          END DO
          IF (ksurf(is) == kpot(npt2)) THEN
            sum0 = sum0 - z0_s(is)*spsurf10(is,jx,jy,jz)*term1
            sumb = sumb - zb_s(is)*spsurf10(is,jx,jy,jz)*term1
          END IF
          fjpotnsurf(idxR0,is,jx,jy,jz) = sum0
          fjpotnsurf(idxRb,is,jx,jy,jz) = sumb
          fjpotnsurf(idxRd,is,jx,jy,jz) = 0.0d0
        END DO

        IF (C2(npt2) == 0.0d0) THEN
          fjpotncomp(idxRb,:,jx,jy,jz) = 0.0d0
          fjpotnsurf(idxRb,:,jx,jy,jz) = 0.0d0
          fjpotncomp(idxRd,:,jx,jy,jz) = fjpotncomp(idxR0,:,jx,jy,jz)
          fjpotnsurf(idxRd,:,jx,jy,jz) = fjpotnsurf(idxR0,:,jx,jy,jz)
        END IF

      END DO

    END DO
  END DO
END DO

RETURN
END SUBROUTINE jacpotential
