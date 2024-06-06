section .data               
;Change Name and Surname for your data.
developer db "_Ivan_ _Gentile_",0

;Constant that is also defined in C.
DimMatrix    equ 4 
SizeMatrix   equ (DimMatrix*DimMatrix) ;=16 

section .text            

;Variables defined in Assembly language.
global developer                        

;Assembly language subroutines called from C.
global showNumberP2, updateBoardP2, copyMatrixP2,
global rotateMatrixRP2, shiftNumbersRP2, addPairsRP2
global readKeyP2, checkEndP2, playP2

;Global variables defined in C.
extern m, mRotated, mAux, mUndo, state

;C functions that are called from assembly code.
extern clearScreen_C,  gotoxyP2_C, getchP2_C, printchP2_C
extern printBoardP2_C, printMessageP2_C, insertTileP2_C   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATTENTION: Remember that in assembly language the variables and parameters 
;; of type 'char' must be assigned to records of type
;; BYTE (1 byte): al, ah, bl, bh, cl, ch, dl, dh, sil, dil, ..., r15b
;; those of type 'short' must be assigned to records of type
;; WORD (2 bytes): ax, bx, cx, dx, si, di, ...., r15w
;; those of type 'int' must be assigned to records of type
;; DWORD (4 bytes): eax, ebx, ecx, edx, esi, edi, ...., r15d
;; those of type 'long' must be assigned to records of type
;; QWORD (8 bytes): rax, rbx, rcx, rdx, rsi, rdi, ...., r15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The assembly subroutines you need to modify to implement pass parameter are:
;;   showNumberP2, updateBoardP2, copyMatrixP2, 
;;   rotateMatrixRP2, shiftNumbersRP2, addPairsRP2
;; The assembly subroutine you need to implement is:
;;   checkEndP2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This subroutine is already done. YOU CANNOT MODIFY IT.
; Place the cursor at a position on the screen.  
; 
; Global variables :	
; None
; 
; Input parameters : 
; rdi(edi): (rowScReen) : Row of the screen where the cursor is placed.
; rsi(esi): (colScreen) :  Column of the screen where the cursor is placed.
; 
; Output parameters: 
; None
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gotoxyP2:
   push rbp
   mov  rbp, rsp
   ; We save the processor's registers' state because 
   ; the C functions do not keep the registers' state.
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15

   ; When we call the gotoxyP2_C function (int rowScreen, int colScreen) from assembly language
   ; the first parameter (rowScreen) must be passed through the rdi (edi) register, and
   ; the second parameter (colScreen) must be passed through the rsi (esi) register.
   call gotoxyP2_C
 
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax

   mov rsp, rbp
   pop rbp
   ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This subroutine is already done. YOU CANNOT MODIFY IT.
; Show a character on the screen at the cursor position.
; 
; Global variables :	
; None
; 
; Input parameters : 
; rdi(dil): (c):  Character to show.
; 
; Output parameters: 
; None
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printchP2:
   push rbp
   mov  rbp, rsp
   ; We save the processor's registers' state because 
   ; the C functions do not keep the registers' state.
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15

   ; When we call the printchP2_C (char c) function from assembly language,
   ; parameter (c) must be passed through the rdi (dil) register.
   call printchP2_C
 
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax

   mov rsp, rbp
   pop rbp
   ret
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This subroutine is already done. YOU CANNOT MODIFY IT.
; Read a character from the keyboard without displaying it 
; on the screen and return it.
; 
; Global variables :	
; None
; 
; Input parameters : 
; None
; 
; Output parameters: 
; rax(al) : (c): Character read from the keyboard.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
getchP2:
   push rbp
   mov  rbp, rsp
   ; We save the processor's registers' state because 
   ; the C functions do not keep the registers' state.
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15
   push rbp
   
   mov rax, 0
   ; When we call the getchP2_C function from assembly language
   ; return over the rax(al) register the read character.
   call getchP2_C

   pop rbp
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   
   mov rsp, rbp
   pop rbp
   ret 


;;;;;
; Converts the number of 6 digits (n <= 999999) stored in the short 
; type variable (n), recived as a parameter, to ASCII characters 
; representing its value.
; If (n) is greater than 999999 we will change the value to 999999.
; The value must be divided (/) by 10, iteratively, 
; until the 6 digits are obtained.
; At each iteration, the remainder of the division (%) which is a value
; between (0-9) indicates the value of the digit to be converted to 
; ASCII ('0' - '9') by adding '0' ( 48 decimal) to be able to display it.
; When the quotient is 0 we will show spaces in the non-significant part.
; For example, if number=103 we will show "103" and not "000103".
; The digits (ASCII character) must be displayed from the position 
; indicated by the variables (rowScreen) and (colScreen), position 
; of the units, to the left.
; The first digit we get is the units, then the tens, ..., to display the
; value the cursor must be moved one position to the left in each iteration.
; To place the cursor call the gotoxyP2 subroutine and to display the 
; characters call the printchP2 subroutine .
; 
; Global variables :	
; None
; 
; Input parameters : 
; rdi(edi): (rScreen): Row to place the cursor on the screen.
; rsi(esi): (cScreen): Column to place the cursor on the screen.
; rdx(edx): (n)      : Number to show.
; 
; Output parameters : 
; None
;;;;;
showNumberP2:
   push rbp
   mov  rbp, rsp
   
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi

   mov eax, edx 

   cmp eax, 999999            ;if (n > 999999)
   jle showNumberP2_IniFor
        mov eax, 999999       ;n = 999999;
   
   showNumberP2_IniFor:
   mov ecx, 0                 ;i=0
   showNumberP2_For:
   cmp ecx, 6                 ;i<6
   jge showNumberP2_End
     mov dl, ' '              ;charac = ' ';
     cmp eax, 0               ;(n > 0)
     jle showNumberP2_Show
       mov edx, 0             
       mov ebx,10   ;n%10; n/10;
       div ebx      ;EAX=EDX:EAX/EBX, EDX=EDX:EAX mod EBX
       add dl,'0'   ;charac = charac + '0';

      showNumberP2_Show:
      call gotoxyP2 ;gotoxyP2_C(rScreen, cScreen);
      push rdi
      mov  dil, dl
      call printchP2;printchP2_C(charac);
      pop  rdi
      
      dec  esi     ;cScreen--;
      inc  ecx     ;i++;
      jmp  showNumberP2_For
  
   showNumberP2_End:
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;
; Update the contents of the Game Board with the data from the 4x4 
; matrix (m) of type short and the scored points (scr), recived as a parameter.
; Go thorugt the entire matrix(m), and for each element of the matrix
; place the cursor on the screen and show the number of that position.
; Go through the entire matrix by rows from left to right and from top to bottom.
; To go through a matrix in assembler the index goes from 0 
; (position [0][0]) to 30 (position [3][3]) with increments of 2 
; because data is short type (WORD) 2 bytes.
; Then, show the scoreboard (scr) at the bottom of the board, 
; row 18, column 26 by calling the showNumberP2 subroutine.
; Finally place the cursor in row 18, column 28 by calling 
; the gotoxyP2 subroutine.
; 
; Global variables :
; (m): Matrix where we have the numbers of the game.
; 
; Input parameters : 
; rdi(edi): (scr): Scored points on the scoreboard.
; 
; Output parameters : 
; None
;;;;;  
updateBoardP2:
   push rbp
   mov  rbp, rsp
   
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9

   mov r8d, edi     ;src;
   mov eax, 0       ;i
   mov ebx, 0       ;j
   mov edi, 0       ;rScreen 
   mov esi, 0       ;cScreen
   mov rcx, 0       ;index to access the matrix m. (0-15)
                    ;índex=(row*DimMatrix)+(column)*4

   ;Loop to show the matrix
   mov edi, 10      ;rScreen = 10;
   mov eax, 0       ;i=0;
   updateBoardP2_fori:
   cmp eax, DimMatrix    ;i<DimMatrix;
   jge updateBoardP2_endfori
      
      mov esi, 17        ;cScreen = 17;
      mov ebx, 0         ;j=0;
      updateBoardP2_forj:
      cmp ebx, DimMatrix ;j<DimMatrix;
      jge updateBoardP2_endforj
         ;mov   edx, 0
         ;mov   dx, WORD[m+rcx]  ;m[i][j];
         movsx  edx, WORD[m+rcx] ;m[i][j];
         call showNumberP2    ;showNumberP2_C(rScreen,cScreen, m[i][j]);

       add esi, 9        ;cScreen = cScreen + 9;
       add rcx, 2        ;index
       inc ebx           ;j++.

      jmp  updateBoardP2_forj
      
      updateBoardP2_endforj:
      
      add edi, 2        ;rScreen = rScreen + 2;
      inc eax           ;i++.
      
      jmp updateBoardP2_fori

   updateBoardP2_endfori:

   mov  edi, 18
   mov  esi, 26
   mov  edx, r8d        ;src;
   call showNumberP2    ;showNumberP2_C(18, 26, scr);   
   mov  esi, 28
   call gotoxyP2        ;gotoxyP1_C(18,28);
   
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx   
   pop rbx
   pop rax

   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Copy the values stored in the (mOrig) matrix, recived as a parameter,
; to the (mDest) matrix, recived as a parameter. The (mRotated) matrix 
; should not be modified, changes should be made to the (m) matrix.
; Go through the entire matrix by rows from left to right and from top to bottom.
; To go through a matrix in assembly language the index goes from 0 
; ([0][0] position) to 30 ([3][3] position) with increments of 2 because
; data is short type (WORD), 2 bytes.
; This will allow to copy two matrixs after a rotation
; and handle the '(u)Undo' option.
; Do not show the matrix.
; 
; Global variables :
; None
; 
; Input parameters : 
; rdi(rdi): (mDest)  : Matrix Address where we have the numbers we want to overwrite.
; rsi(rsi): (mOrigin): Matrix address where we have the numbers of the game.
; 
; Output parameters : 
; None.
;;;;;  
copyMatrixP2:
   push rbp
   mov  rbp, rsp

   push rax
   push rbx
   push rsi
   push rdi
   push r8
   push r9

   mov r8, rdi           ;Address of the destinatio matrix.
   mov r9, rsi           ;Address of the origin matrix.

   mov rax, 0            ;index
   mov rdi, 0            ;i=0
   copyMatrixP2_fori:
   cmp rdi, DimMatrix    ;i<DimMatrix
   jge copyMatrixP2_endfori 
      
      mov rsi, 0         ;j=0
      copyMatrixP2_forj:
      cmp rsi, DimMatrix ;j<DimMatrix
      jge copyMatrixP2_endforj
         
      
      
      mov bx, WORD[r9+rax]    ;mOrig[i][j]; 
      mov WORD[r8+rax], bx    ;mDest[i][j]=mOrig[i][j];
      
      add rax, 2
      inc rsi            ;j++

      jmp  copyMatrixP2_forj
      
      copyMatrixP2_endforj:
      
      inc rdi            ;i++
      
      jmp copyMatrixP2_fori

   copyMatrixP2_endfori:
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rbx 
   pop rax        

   mov rsp, rbp
   pop rbp
   ret   
   

;;;;;      
; Rotate the matrix (mToRotate), recived as a parameter, to the right 
; over the matrix (mRotated).
; The first row becomes the fourth column, the second row becomes 
; the third column, the third row becomes the second column, 
; and the fourth row becomes the first column.
; In the .pdf file there are a detailed explanation how to do the rotation.
; NOTE: This is NOT the same as transpose the matrix.
; The matrix (mToRotate) should not be modified, changes should be made
; to the (mRotated) matrix.
; To go through a matrix in assembly language the index goes from 0 
; (position [0][0]) to 30 (position [3][3]) with increments of 2 
; because the data is short type (WORD), 2 bytes.
; To access a specific position of a matrix in assembly language, 
; you must take into account that the index is:
; (index=(row*DimMatrix+column)*2),
; we multiply by 2 because the data is short type (WORD), 2 bytes.
; Once the rotation has been done, copy the (mRotated) matrix  over the
; matrix recived as a parameter by calling the copyMatrixP2 subroutine.
; The matrix should not be displayed.
; 
; Global variables :
; (mRotated) : Matrix where we have the numbers of the game.
; 
; Input parameters : 
; rdi(rdi): (mToRotate): Matrix address where we have the numbers of the game rotated to the right.
; 
; Output parameters : 
; None
;;;;;  
rotateMatrixRP2:
   push rbp
   mov  rbp, rsp
   
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   
   mov rdx, rdi               ;short mToRotate[DimMatrix][DimMatrix] 
   mov r10, 0                 ; [i][j]
   mov r8d, 0                 ;i=0;
   rotateMatrixRP1_fori:
   cmp r8d, DimMatrix         ;i<DimMatrix;
   jge rotateMatrixRP1_endfori 
      
      mov r9d, 0              ;j=0;
      rotateMatrixRP1_forj:
      cmp r9d, DimMatrix      ;j<DimMatrix;
      jge rotateMatrixRP1_endforj
                              
      mov r11, r9             ;r11 = j
      shl r11, DimMatrix/2    ;r11 = j*DimMatrix     (DimMatrix=4)
      add r11, DimMatrix      ;r11 = j*DimMatrix+(DimMatrix)
      dec r11                 ;r11 = j*DimMatrix+(DimMatrix-1)
      sub r11, r8             ;r11 = j*DimMatrix+(DimMatrix-1-i)
      shl r11, 1              ;r11 = j*DimMatrix+(DimMatrix-1-i)*2

      mov bx, WORD[rdx+r10]       ;mToRotate[i][j];
      mov WORD[mRotated+r11], bx  ;mRotated[j][DimMatrix-1-i] = m[i][j]
      
      add r10, 2              ;index+2
      inc r9                  ;j++.
      jmp  rotateMatrixRP1_forj
      
      rotateMatrixRP1_endforj:
      inc r8                  ;i++.
      jmp rotateMatrixRP1_fori

   rotateMatrixRP1_endfori:
   
   ;mov  rdi, rdx
   mov  rsi, mRotated
   call copyMatrixP2          ;copyMatrixP2_C(mToRotate, mRotated);
   
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Shift right the numbers in each row of the matrix (mShift),
; recived as a parameter, keeping the order of the numbers and moving 
; the zeros to the left.
; Go through the matrix by rows from right to left and bottom to top.
; To go through a matrix in assembly language, in this case, the index 
; goes from 30 (position [3][3]) to 0 (position [0][0]) with increments
; of 2 because the data is short type (WORD), 2 bytes.
; To access a specific position of a matrix in assembly language, 
; you must take into account that the index is:
; (index=(row*DimMatrix+column)*2),
; we multiply by 2 because the data is short type (WORD), 2 bytes.
; If a number is moved (NOT THE ZEROS) the shifts must be counted by 
; increasing the variable (shifts).
; In each row, if a 0 is found, check if there is a non-zero number 
; in the same row to move it to that position.
; If a row of the matrix is: [0,2,0,4] and (shifts=0), 
; it will be [0,0,2,4] and (shifts=2).
; Changes must be made on the same matrix.
; The matrix should not be displayed.
; 
; Global variables :
; None
; 
; Input parameters : 
; rdi(edi): (mShift): Matrix address where we have the numbers of the game to shift.
; 
; Output parameters : 
; rax(eax): (shifts): Shifts that have been made.
;;;;;  
shiftNumbersRP2:
   push rbp
   mov  rbp, rsp

   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   
   mov rdx, rdi                    ;Adreça de la matriu
   mov r11, 0                      ;shifts=0;
   
   mov rax, (SizeMatrix-1)*2       ;index [i][j]
   mov rcx, 0                      ;index [i][k]
    
   mov r8, DimMatrix               ;i = DimMatrix
   dec r8                          ;i = DimMatrix-1
   shiftNumbersRP2_Rows:
   
      mov r9 , DimMatrix           ;j = DimMatrix
      dec r9                       ;j = DimMatrix-1
      shiftNumbersRP2_Cols:
      mov rax, r8                  ;rax = i
      shl rax, (DimMatrix/2)       ;rax = i*DimMatrix    (DimMatrix=4)
      add rax, r9                  ;rax = i*DimMatrix+j
      shl rax, 1                   ;rax = (i*DimMatrix+j)*2
      cmp WORD[rdx+rax], 0         ;if (m[i][j] == 0)
      jne shiftNumbersRP2_IsZero
         mov r10, r9
         dec r10                   ;k = j-1;
         mov rcx, r8               ;rcx = i
         shl rcx, (DimMatrix/2)    ;rcx = i*DimMatrix    (DimMatrix=4)
         add rcx, r10              ;rcx = i*DimMatrix+k
         shl rcx, 1                ;rcx = (i*DimMatrix+k)*2
         shiftNumbersRP2_While:
         cmp r10, 0                ;k>=0
         jl  shiftNumbersRP2_EndWhile
            cmp WORD[rdx+rcx], 0   ;m[i][k]==0
            jne shiftNumbersRP2_EndWhile
               sub rcx, 2
               dec r10             ;k--;
            jmp shiftNumbersRP2_While
         shiftNumbersRP2_EndWhile:
         cmp r10, -1               ;k==-1
         je shiftNumbersRP2_Next
            mov bx, WORD[rdx+rcx]  ;m[i][k]
            mov WORD[rdx+rcx], 0   ;m[i][k]= 0
            mov WORD[rdx+rax], bx  ;m[i][j]=m[i][k]
            inc r11                ;shifts++; 
      shiftNumbersRP2_IsZero:
      sub rax, 2
      dec r9
      cmp r9, 0                    ;j>0
      jg  shiftNumbersRP2_Cols
   shiftNumbersRP2_Next:
   dec r8
   cmp r8, 0                       ;i>=0
   jge  shiftNumbersRP2_Rows

   shiftNumbersRP2_End:  
   mov eax, r11d                   ;return shifts;

   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx         
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Find pairs of the same number from the right of the matrix (mPairs), 
; recived as a parameter, and accumulate the points on the scoreboard 
; by adding the points of the pairs that have been made.
; Go through the matrix by rows from right to left and from bottom to top.
; When a pair is found, two consecutive tiles in the same
; row with the same number, join the pair by adding the values and 
; store the sum in the right tile, a 0 in the left tile and
; accumulate this sum in the (p) variable (earned points).
; If a row of the matrix is: [8,4,4,2] it will be [8,0,8,2] and
; p = p + (4+4).
; Return the points (p) obtained from making pairs.
; To go through a matrix in assembly language, in this case, the index 
; goes from 30 (position [3][3]) to 0 (position [0][0]) with increments
; of 2 because the data is short type (WORD), 2 bytes.
; Changes must be made on the same matrix.
; The matrix should not be displayed.
; 
; Global variables :
; None
; 
; Input parameters : 
; rdi(edi): (mPairs): Matrix address where we have the numbers of the game to make pairs.
; 
; Output parameters : 
; rax(eax): (p): Points to add to the scoreboard.
;;;;;  
addPairsRP2:
   push rbp
   mov  rbp, rsp

   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   
   mov rdx , rdi                   ;short mPairs[DimMatrix][DimMatrix]
   
   mov r10, 0                      ;p = 0
   mov rax, (SizeMatrix-1)*2       ;index [i][j]
   mov rbx, 0
   mov rcx, (SizeMatrix-1)*2       ;index [i][j-1]

   mov r8, DimMatrix               ;i = DimMatrix
   dec r8                          ;i = DimMatrix-1
   addPairsRP2_Rows:
   mov r9, DimMatrix               ;j = DimMatrix
   dec r9                          ;j = DimMatrix-1
      addPairsRP2_Cols:
      mov bx, WORD[rdx+rax]         
      cmp bx, 0                    ;mPairs[i][j]!=0
      je  addPairsRP2_EndIf
      mov rcx, rax
      sub rcx, 2
      cmp bx, WORD[rdx+rcx]        ;mPairs[i][j]==mPairs[i][j-1]
      jne addPairsRP2_EndIf
         shl bx,1                  ;mPairs[i][j]*2
         mov WORD[rdx+rax], bx     ;mPairs[i][j]  = mPairs[i][j]*2
         mov WORD[rdx+rcx], 0      ;mPairs[i][j-1]= 0
         add r10w, bx              ;p = p + mPairs[i][j]
      addPairsRP2_EndIf:
      sub rax, 2
      dec r9                       ;j--
      cmp r9, 0                    ;j>0
      jg  addPairsRP2_Cols
   sub rax, 2
   dec r8                          ;i--
   cmp r8, 0                       ;i>=0
   jge  addPairsRP2_Rows
   
   mov rax, 0  
   movsx eax, r10w                 ;return p;

   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Check if a 2048 has been reached or if no move can be made.
; If there is the number 2048 in the matrix (m), change the status 
; to 4 (status='4') to indicate that the game has been won (WIN!).
; If we haven't won, check if we can make a move,
; If no move can be made change the status to 5 (status='5') 
; to indicate that the game has been lost (GAME OVER!!!).
; Go through the matrix (m) row by row from right to left and 
; from bottom to top counting the empty tiles and checking if 
; the number 2048 is there.
; To go through a matrix in assembly language, in this case, the index 
; goes from 30 (position [3][3]) to 0 (position [0][0]) with increments
; of 2 because the data is short type (WORD), 2 bytes.
; If there isn't any 2048 assign (status='4') and finish.
; If there is no number 2048 and there are no empty tiles, 
; see if you can make an horizontal or a vertical move. 
; To do this, you must copy the matrix (m) over the matrix (mAux) 
; by calling (copyMatrixP2), make pairs over the matrix (mAux) 
; to see if you can make pairs horizontally by calling (addPairsRP2)
; and save the points obtained , rotate the matrix (mAux) by calling
; (rotateMatrixRP2) and make pairs in the matrix (mAux) again 
; to see if you can make pairs vertically by calling (addPairsRP2) 
; and accumulate the points obtained with the points obtained before, 
; if the accumulated points are 0, it means no pairs can be made 
; and the game status must be set to 5 (status='5'), Game Over!.
; Neither the (m) matrix nor the (mUndo) matrix can be modified.
; 
; Global variables :
; (m)       : Matrix where we have the numbers of the game.
; (mRotated): Matrix where we have the numbers of the game rotated.
; (mAux)    : Matrix where we have the numbers of the game to check it.
; (state)   : State of the game.
; 
; Input parameters : 
; None
; 
; Output parameters : 
; None
;;;;;  
checkEndP2:
   push rbp
   mov  rbp, rsp

   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
 
   mov ebx, 0                      ;zeros=0;
   mov ecx, 0                      ;pairs=0;
   
   mov rax, (SizeMatrix)*2
   mov r8, DimMatrix               ;i = DimMatrix;
   checkEndP2_Rows: 
      dec r8                       ;i--;
      mov r9 , DimMatrix           ;j = DimMatrix;
      checkEndP2_Cols:
         dec r9                    ;j--
         sub rax,2
         cmp WORD[m+rax], 0        ;(m[i][j] == 0)
         jne checkEndP2_Win
            inc ebx                ;zeros++;
         checkEndP2_Win:
         cmp WORD[m+rax], 2048     ;m[i][j] == 2048
         jne checkEndP2_Next
            mov BYTE[state], '4'   ;state = '4'
            jmp checkEndP2_End
            
      checkEndP2_Next:   
      cmp r9, 0                    ;j>0
      jg   checkEndP2_Cols
   cmp r8, 0                       ;i>0
   jg   checkEndP2_Rows

   cmp ebx, 0                      ;(zeros == 0)
   jne checkEndP2_End
      cmp  BYTE[state], '4'        ;(state!= '4')
      je  checkEndP2_End
      mov  rdi, mAux
      mov  rsi, m
      call copyMatrixP2            ;copyMatrixP2_C(mAux,m);
      call addPairsRP2             ;addPairsRP2_C(mAux);
      mov  ecx, eax                ;pairs = addPairsRP2_C(mAux)
      call rotateMatrixRP2         ;rotateMatrixRP2_C(mAux)
      call addPairsRP2             ;addPairsRP2_C(mAux)
      add  ecx, eax                ;pairs = pairs + addPairsRP2_C(mAux)
      cmp  ecx, 0                  ;pairs==0
      jne  checkEndP2_End      
         mov BYTE[state], '5'      ;state = '5'
      
   checkEndP2_End:

   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax
   
   mov rsp, rbp
   pop rbp
   ret   



;;;;;
; This subroutine is already done. YOU CANNOT MODIFY IT.
; Read a key by calling the getchP2 subroutine and it is stored
; in the (al) register.
; According to the key read we will call the corresponding functions.
; ['i' (up), 'j' (left), 'k' (down) or 'l' (right)]
; Move the numbers and make the pairs according to the chosen direction.
; Depending on the key pressed, rotate the matrix (m) by calling 
; (rotateMatrixRP1), to be able to move the numbers to the right 
; (shiftNumbersRP1), make pairs to the right (addPairsRP1) and 
; shift numbers to the right again (shiftNumbersRP1) with the pairs made, 
; If a move or a pair has been made, indicate this by assigning (state='2').
; Then, keep rotating the matrix by calling (rotateMatrixRP1) 
; until leaving the matrix in the initial position.
; For the 'l' key (right) no rotations are required, for the rest, 
; 4 rotations must be made.
; 'u'                Assign (state = '3') to undo the last move.
; '<ESC>' (ASCII 27) Set (state = '0') to exit the game.
; If it is not any of these keys do nothing.
; The changes produced by these subroutine are not displayed on the screen.
; 
; Global variables :
; (mRotated): Matrix where we have the numbers of the game rotated.
; (m)       : Matrix where we have the numbers of the game.
; (state)   : State of the game.
; 
; Input parameters : 
; rdi(edi): (actualScore): Scored points on the scoreboard.
; 
; Output parameters : 
; rax(eax): (actualScore): Updated scored points.
;;;;;  
readKeyP2:
   push rbp
   mov  rbp, rsp

   push rbx
   push rdx
   push rsi
   push rdi
   push r8          ;s1
   push r9          ;s2
   push r10         ;p
   push r11         ;actualscore
   
   mov  r11d, edi
   mov  rdi, m      
   mov  rsi, mRotated
   
   call getchP2     ;getchP2_C();
      
   readKeyP2_i:
   cmp al, 'i'                ;i:(105) up
   jne  readKeyP2_j
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      
      call shiftNumbersRP2    ;s1 = shiftNumbersRP2_C(m);
      mov  r8d, eax
      call addPairsRP2        ;p  = addPairsRP2_C(m);
      mov  r10d, eax
      call shiftNumbersRP2    ;s2 = shiftNumbersRP2_C(m);
      mov  r9d, eax           
      
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      jmp  readKeyP2_moved
      
   readKeyP2_j:
   cmp al, 'j'                ;j:(106) left
   jne  readKeyP2_k
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      
      call shiftNumbersRP2    ;s1 = shiftNumbersRP2_C(m);
      mov  r8d, eax
      call addPairsRP2        ;actualScore = actualScore + p;
      mov  r10d, eax
      call shiftNumbersRP2    ;s2 = shiftNumbersRP2_C(m);
      mov  r9d, eax          
      
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      jmp  readKeyP2_moved
   
   readKeyP2_k:
   cmp al, 'k'                ;k:(107) down
   jne  readKeyP2_l
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
     
      call shiftNumbersRP2    ;s1 = shiftNumbersRP2_C(m);
      mov  r8d, eax
      call addPairsRP2        ;p  = addPairsRP2_C(m);
      mov  r10d, eax
      call shiftNumbersRP2    ;s2 = shiftNumbersRP2_C(m);
      mov  r9d, eax           
      
      call rotateMatrixRP2    ;rotateMatrixRP2_C(m);
      jmp  readKeyP2_moved
      
   readKeyP2_l:
   cmp al, 'l'                ;l:(108) right
   jne  readKeyP2_u
      
      call shiftNumbersRP2    ;s1 = shiftNumbersRP2_C(m);
      mov  r8d, eax
      call addPairsRP2        ;p  = addPairsRP2_C(m);
      mov  r10d, eax
      call shiftNumbersRP2    ;s2 = shiftNumbersRP2_C(m);
      mov  r9d, eax           
      jmp readKeyP2_moved
      
   readKeyP2_u:
   cmp al, 'u'                ; Undo
   jne  readKeyP2_ESC
      mov BYTE[state], '3'    ;state = '3';
      jmp  readKeyP2_End
   
   readKeyP2_ESC:
   cmp al, 27                 ; Sortir del programa
   jne readKeyP2_End
      mov BYTE[state], '0'    ;state='0';
   jmp readKeyP2_End 

   readKeyP2_moved:
   add  r11d, r10d            ;actualScore = actualScore + p;
   cmp  r8d, 0                ;if ( (s1>0) || 
   jg  readKeyP2_status2
      cmp  r10d, 0            ;(p>0) || 
      jg  readKeyP2_status2
         cmp r9d, 0           ;(s2>0) ) 
         jg  readKeyP2_status2
            jmp readKeyP2_End
   readKeyP2_status2:         ;state = '2';
   mov  BYTE[state], '2'
      
   readKeyP2_End:
   mov eax, r11d              ;return actualScore;
   
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rbx
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This subroutine is already done. YOU CANNOT MODIFY IT.
; Show a message below the dashboard according to the value of 
; the (state) variable by calling the printMessageP2 subroutine.
; 
; Global variables :
; (state):  State of the game.
; 
; Input parameters : 
; None
; 
; Output parameters : 
; None
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printMessageP2:
   push rbp
   mov  rbp, rsp
   ; We save the processor's registers' state because 
   ; the C functions do not keep the registers' state.
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15

   ; We call the printMessageP2_C function from assembly language.
   call printMessageP2_C
 
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax
   
   mov rsp, rbp
   pop rbp
   ret 
   
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This subroutine is already done. YOU CANNOT MODIFY IT.
; Generar nova fitxa de forma aleatòria.
; Si hi ha com a mínim una casella buida a la matriu (m) genera una 
; fila i una columna de forma aleatòria fins que és una de les caselles 
; buides. A continuació generar un nombre aleatori per decidir si la 
; nova fitxa ha de ser un 2 (90% dels casos) o un 4 (10% dels casos),
; cridant la funció insertTileP2_C().
; 
; Global variables :	
; Cap
; 
; Input parameters : 
; Cap
; 
; Output parameters : 
; Cap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
insertTileP2:
   push rbp
   mov  rbp, rsp
   ; We save the processor's registers' state because 
   ; the C functions do not keep the registers' state.
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15

   ; We call the insertTileP2_C function from assembly language.
   call insertTileP2_C
 
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax
   
   mov rsp, rbp
   pop rbp
   ret 
   
   
;;;;;
; This subroutine is already done. YOU CANNOT MODIFY IT.
; 2048 game.
; Main function of the game
; Allows you to play the 8-PUZZLE game by calling all the functionalities.
;
; Pseudo code:
; Initialize state of the game, (state='1')
; Clear screen (call the clearScreen_C function).
; Display the board (call function printBoardP1_C).
; Updates the content of the board and the score (call updateBoardP1 subroutine).
; While (state=='1') do
;   Copy the matrix (m) over the matrix (mAux) (by calling the subroutine
;   (copyMatrixP2) and copy the points (score) over (scoreAux).
;   Read a key (call the readKeyP1 subroutine) 
;   and call the corresponding subroutines.
;   If we have moved some number when making the shifts or when making 
;   pairs (state=='2'), copy the state of the game we saved before
;   (mAux and scoreAux) over (mUndo and scoreUndo) to be able to undo 
;   the last move (recover previous state) by copying (mAux) over (mUndo)
;   (calling copyMatrixP2 subroutine) and copying (scoreAux) over (scoreUndo).
;   Generate a new tile (calling the insertTileP2 subroutine) and set 
;   the state variable to '1' (state='1').
;   If we need to recover the previous state (state='3'), copy the 
;   previous state of the game we have in (mUndu and scoreUndu) over 
;   (m and score) (calling the copyMatrixP2 subroutine) and copying 
;   (scoreUndu) over (score) and set the state variable to '1' (state='1').
;   Updates the board content and the score (call updateBoardP2 subroutine).
;   Check if 2048 has been reached or if no move can be made
;   (call CheckEndP2 subroutine).
;   Display a message below the board on the value of the variable
;   (state) (call printMessageP2 subroutine).
; End while 
; Exit:
; The game is over.
; 
; Global variables :
; (m)       : Matrix where we have the numbers of the game.
; (mRotated): Matrix where we have the numbers of the game rotated.
; (mAux)    : Matrix where we have the numbers of the game to check it.
; (mAux)    : Matrix where we have the numbers of the game to undu the last move.
; (state)   : State of the game.
;             '0': Exit, 'ESC' pressed.
;             '1': Let's keep playing.
;             '2': We continue playing but there have been changes in the matrix.
;             '3': Undo last move.
;             '4': Win, 2048 has been reached.
;             '5': Lost, no more moves.
; 
; Input parameters : 
; None
; 
; Output parameters : 
; None
;;;;;  
playP2:
   push rbp
   mov  rbp, rsp
   
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   
   call clearScreen_C
   call printBoardP2_C
   
   mov  r10d, 290500        ;int score     = 290500;
   mov  r11d, 0             ;int scoreAux  = 0;
   mov  r12d, 1             ;int scoreUndu = 1;
   
   mov  BYTE[state], '1'    ;state = '1';	   		
   
   mov  edi, r10d
   call updateBoardP2

   playP2_Loop:                    ;while  {  
   cmp  BYTE[state], '1'           ;(state == 1)
   jne  playP2_End
      
      mov edi, mAux
      mov esi, m   
      call copyMatrixP2            ;copyMatrixP2_C(mAux,m);
      mov r11d, r10d               ;scoreAux = score
      mov edi,  r10d                        
      call readKeyP2               ;readKeyP2_C();
      mov r10d, eax
      cmp BYTE[state], '2'         ;(state == '2') 
      jne playP2_Next 
         mov edi, mUndo
         mov esi, mAux
         call copyMatrixP2         ;copyMatrixP2_C(mUndo,mAux);
         mov  r12d, r11d           ;scoreUndo = scoreAux
         call insertTileP2         ;insertTileP2_C(); 
         mov BYTE[state],'1'       ;state = '1';
         jmp playP2_Next
      cmp BYTE[state], '3'         ;(state == '3') 
      jne playP2_Next  
         mov  edi, m 
		 mov  esi, mUndo
		 call copyMatrixP2         ;copyMatrixP2_C(m,mUndo);
		 mov  r10d, r12d           ;score = scoreUndo;
		 mov  BYTE[state], '1'     ;state = '1';
      playP2_Next:
      mov  edi, r10d
      call updateBoardP2           ;updateBoardP2_C(score);
      call checkEndP2              ;checkEndP2_C();  
      call printMessageP2          ;printMessageP2_C(); 
      cmp BYTE[state], '3'         ;(state == '3') 
      jne playP2_Loop
         mov edi, m
		 mov  esi, mUndo
		 call copyMatrixP2        ;copyMatrixP2_C(m,mUndo);
		 mov  r10d, r12d          ;score = scoreUndo;
		 mov  BYTE[state], '1'    ;state = '1';
		 mov  edi, r10d
		 call updateBoardP2
   jmp playP2_Loop
   
   playP2_End:
           
   
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax  
   
   mov rsp, rbp
   pop rbp
   ret

