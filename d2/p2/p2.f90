program d1
    use safe
    ! importante pra dizer q as vars vão ser todas declaradas explicitamente
    implicit none 
    
    integer :: tam                          ! tamanho da entrada
    character(len = 128) :: buf             ! buffer
    integer :: ios                          ! controle leitor buffer
    integer :: rep(10), modrep(9)           ! report e report modificado
    
    integer :: input, stat                  ! variaveis controle p/ abrir os arq
    character(len=512) :: msg
    
    integer :: i                            ! iteradores
    logical :: s                            ! controle safe
    integer :: safes                        ! numero de safes (resposta)
    
    
    ! abrindo a lista
    open(newunit=input, file="../input", status="old", action="read", iostat=stat, iomsg=msg)
    if (stat /= 0) then
        print *, trim(msg)
        error stop
    end if
    
    safes = 0
    
    do while (.true.)
        ! le uma linha no buffer
        read(input, '(a)', END = 200) buf
        
        ! seta todos os elementos em -1
        rep = -1
        
        ! le os numeros da linha
        read(buf, *, iostat = ios) rep
        
        ! testa se esta safe sem mudar nada
        s = isSafe(rep)
        
        i = 1
        modrep = rep(2:)
        ! se nao, cria um report modificado retirando um dos levels por vez
        ! se retirar um deles torna o report safe, sai do loop 
        do while ((.not. s) .and. rep(i) > -1 .and. i < size(rep))
            s = isSafe(modrep)
            modrep(i) = rep(i)
            i = i + 1
        end do
            
        ! se este eh safe, incrementa o numero de reports safes
        if (s) then
            safes = safes + 1
        end if
        
    end do
    
    200 CONTINUE
    
    close(input)
    
    print *, safes
    
end program d1
