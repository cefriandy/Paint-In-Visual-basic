Imports System.Drawing.Imaging
Imports System.IO

Module Module1
    Public bmp1 As New Bitmap(562, 338, PixelFormat.Format32bppArgb)
    Public defCol As Color
    Public showOrder As Boolean
    Public showPixelOrder As Boolean
    Public overFlow As Integer
    Public notTakingTheload As Boolean

    Structure TProcess
        Dim type As Integer
        Dim L As TLine
        Dim C As TCircle
        Dim P As TColor
    End Structure

    Structure TLine
        Dim x1 As Integer
        Dim y1 As Integer
        Dim x2 As Integer
        Dim y2 As Integer
        Dim thickNess As Integer
        Dim col As Color
    End Structure

    Structure TCircle
        Dim xc As Integer
        Dim yc As Integer
        Dim r As Integer
        Dim thickNess As Integer
        Dim col As Color
    End Structure

    Structure TColor
        Dim X As Integer
        Dim Y As Integer
        Dim type As Integer
        Dim col As Color
        Dim bOund As Color
    End Structure

    Structure TPoint
        Dim X As Integer
        Dim Y As Integer
        Dim C As Color
        Dim span As Boolean
    End Structure

    Structure TStackPoint
        Dim eLmt() As TPoint
        Dim N As Integer
    End Structure

    Function isEmptyStack(S As TStackPoint) As Boolean
        Return S.N = 0
    End Function

    Function abs(N As Integer) As Integer
        If N < 0 Then
            N = N * -1
        Else 'N >= 0
            N = N * 1
        End If
        Return N
    End Function

    Function GetPixelColor(pic As PictureBox, X As Integer, Y As Integer) As Color
        Dim MyColor As Color
        Using MyBMP As New Bitmap(pic.Image)
            MyColor = MyBMP.GetPixel(X, Y)
        End Using
        Return MyColor
    End Function

    Sub initLine(ByRef L As TLine)
        L.x1 = 0
        L.y1 = 0
        L.x2 = 0
        L.y2 = 0
        L.thickNess = 5
        L.col = defCol
    End Sub

    Sub initCircle(ByRef C As TCircle)
        C.xc = 0
        C.yc = 0
        C.r = 0
        C.col = defCol
        C.thickNess = 5
    End Sub

    Sub initPixel(ByRef P As TColor)
        P.X = 0
        P.Y = 0
        P.col = defCol
        P.bOund = defCol
        P.type = 1
    End Sub

    Sub initStack(ByRef S As TStackPoint)
        S.N = 0
        ReDim S.eLmt(S.N - 1)
    End Sub

    Sub push(ByRef S As TStackPoint, X As Integer, Y As Integer)
        S.N = S.N + 1
        ReDim Preserve S.eLmt(S.N + 1)
        S.eLmt(S.N).X = X
        S.eLmt(S.N).Y = Y
    End Sub

    Sub pop(ByRef S As TStackPoint, ByRef X As Integer, ByRef Y As Integer)
        X = S.eLmt(S.N).X
        Y = S.eLmt(S.N).Y
        ReDim Preserve S.eLmt(S.N - 1)
        S.N = S.N - 1
    End Sub

    Sub push(ByRef S As TStackPoint, X As Integer, Y As Integer, span As Boolean)
        S.N = S.N + 1
        ReDim Preserve S.eLmt(S.N + 1)
        S.eLmt(S.N).X = X
        S.eLmt(S.N).Y = Y
        S.eLmt(S.N).span = span
    End Sub

    Sub pop(ByRef S As TStackPoint, ByRef X As Integer, ByRef Y As Integer, ByRef span As Boolean)
        X = S.eLmt(S.N).X
        Y = S.eLmt(S.N).Y
        span = S.eLmt(S.N).span
        ReDim Preserve S.eLmt(S.N - 1)
        S.N = S.N - 1
    End Sub

    Sub createStraightLine(L As TLine, picturebox1 As PictureBox)
        Dim gBmp As Graphics = Graphics.FromImage(bmp1)
        Dim myBrush As New SolidBrush(L.col)
        Dim dx, dy, d, dr, dur, x, y As Integer
        'init
        dx = L.x2 - L.x1
        dy = L.y2 - L.y1
        L.col = Color.Black
        'f.e
        If L.x1 <= L.x2 And L.y1 <= L.y2 Then
            If dx >= dy Then
                dr = 2 * dy
                dur = 2 * (dy - dx)
                d = 2 * dy - dx
            Else 'dy < dx
                dr = 2 * dx
                dur = 2 * (dx - dy)
                d = 2 * dx - dy
            End If
        ElseIf L.x1 > L.x2 And L.y1 <= L.y2 Then
            If abs(dx) >= abs(dy) Then
                dr = -2 * dy
                dur = -2 * (dx + dy)
                d = -dx - 2 * dy
            Else 'dy < dx
                dr = -2 * dx
                dur = -2 * (dy + dx)
                d = -dy - 2 * dx
            End If
        ElseIf L.x1 <= L.x2 And L.y1 > L.y2 Then
            If abs(dx) >= abs(dy) Then
                dr = 2 * dy
                dur = 2 * (dy + dx)
                d = 2 * dy + dx
            Else 'dy < dx
                dr = 2 * dx
                dur = 2 * (dx + dy)
                d = 2 * dx + dy
            End If
        ElseIf L.x1 > L.x2 And L.y1 > L.y2 Then
            If abs(dx) >= abs(dy) Then
                dr = -2 * dy
                dur = -2 * (dy - dx)
                d = dx - 2 * dy
            Else 'dy < dx
                dr = -2 * dx
                dur = -2 * (dx - dy)
                d = -dx - 2 * dy
            End If
        End If
        y = L.y1
        x = L.x1
        dx = abs(dx)
        dy = abs(dy)
        'dim point1 as new point(x1 * 20, y1 * 20)
        If L.x1 > 0 And L.x2 > 0 And L.y1 > 0 And L.y2 > 0 Then
            If L.x1 <= L.x2 And L.y1 <= L.y2 Then
                If dx >= dy Then
                    While x < L.x2
                        gBmp.FillEllipse(myBrush, x, y, L.thickNess, L.thickNess)
                        'dim point2 as new point(x * 20, y * 20)
                        'e.graphics.drawline(pens.black, point1, point2)
                        'point1 = new point(x * 20, y * 20)
                        If d <= 0 Then
                            'd <= 0 choose r
                            d = d + dr
                        Else 'd>0, choose ur
                            d = d + dur
                            y = y + 1
                        End If
                        x += 1
                    End While
                Else 'dy < dx
                    While y < L.y2
                        gBmp.FillEllipse(myBrush, x, y, L.thickNess, L.thickNess)
                        'dim point2 as new point(x * 20, y * 20)
                        'e.graphics.drawline(pens.black, point1, point2)
                        'point1 = new point(x * 20, y * 20)
                        If d <= 0 Then
                            'd <= 0 choose r
                            d = d + dr
                        Else 'd>0, choose ur
                            d = d + dur
                            x = x + 1
                        End If
                        y += 1
                    End While
                End If
            ElseIf L.x1 > L.x2 And L.y1 <= L.y2 Then
                If dx >= dy Then
                    While x > L.x2
                        gBmp.FillEllipse(myBrush, x, y, L.thickNess, L.thickNess)
                        'dim point2 as new point(x * 20, y * 20)
                        'e.graphics.drawline(pens.black, point1, point2)
                        'point1 = new point(x * 20, y * 20)
                        If d > 0 Then
                            'd <= 0 choose r
                            d = d + dr
                        Else 'd>0, choose ur
                            d = d + dur
                            y = y + 1
                        End If
                        x -= 1
                    End While
                Else 'dy < dx
                    While y < L.y2
                        gBmp.FillEllipse(myBrush, x, y, L.thickNess, L.thickNess)
                        'dim point2 as new point(x * 20, y * 20)
                        'e.graphics.drawline(pens.black, point1, point2)
                        'point1 = new point(x * 20, y * 20)
                        If d <= 0 Then
                            'd <= 0 choose r
                            d = d + dr
                        Else 'd>0, choose ur
                            d = d + dur
                            x = x - 1
                        End If
                        y += 1
                    End While
                End If
            ElseIf L.x1 <= L.x2 And L.y1 > L.y2 Then
                If dx >= dy Then
                    While x < L.x2
                        gBmp.FillEllipse(myBrush, x, y, L.thickNess, L.thickNess)
                        'dim point2 as new point(x * 20, y * 20)
                        'e.graphics.drawline(pens.black, point1, point2)
                        'point1 = new point(x * 20, y * 20)
                        If d > 0 Then
                            'd <= 0 choose r
                            d = d + dr
                        Else 'd>0, choose ur
                            d = d + dur
                            y = y - 1
                        End If
                        x += 1
                    End While
                Else 'dy > dx
                    While y > L.y2
                        gBmp.FillEllipse(myBrush, x, y, L.thickNess, L.thickNess)
                        'dim point2 as new point(x * 20, y * 20)
                        'e.graphics.drawline(pens.black, point1, point2)
                        'point1 = new point(x * 20, y * 20)
                        If d <= 0 Then
                            'd <= 0 choose r
                            d = d + dr
                        Else 'd>0, choose ur
                            d = d + dur
                            x = x + 1
                        End If
                        y -= 1
                    End While
                End If
            ElseIf L.x1 > L.x2 And L.y1 > L.y2 Then
                If dx >= dy Then
                    While x > L.x2
                        gBmp.FillEllipse(myBrush, x, y, L.thickNess, L.thickNess)
                        'dim point2 as new point(x * 20, y * 20)
                        'e.graphics.drawline(pens.black, point1, point2)
                        'point1 = new point(x * 20, y * 20)
                        If d <= 0 Then
                            'd <= 0 choose r
                            d = d + dr
                        Else 'd>0, choose ur
                            d = d + dur
                            y = y - 1
                        End If
                        x -= 1
                    End While
                Else 'dy > dx
                    While y > L.y2
                        gBmp.FillEllipse(myBrush, x, y, L.thickNess, L.thickNess)
                        'dim point2 as new point(x * 20, y * 20)
                        'e.graphics.drawline(pens.black, point1, point2)
                        'point1 = new point(x * 20, y * 20)
                        If d <= 0 Then
                            'd <= 0 choose r
                            d = d + dr
                        Else 'd>0, choose ur
                            d = d + dur
                            x = x - 1
                        End If
                        y -= 1
                    End While
                End If
            End If
        End If
        gBmp.Dispose()
    End Sub

    Sub createCircle(C As TCircle, picturebox1 As PictureBox)
        Dim x, y, d, dR, dDR As Integer
        Dim gBmp As Graphics = Graphics.FromImage(bmp1)
        Dim myBrush As New SolidBrush(C.col)
        'init
        x = 0
        y = C.r
        d = 1 - C.r
        dR = 2 * x + 2
        dDR = 2 * (x - y) + 1
        If x < y Then
            gBmp.FillEllipse(myBrush, C.xc + x, C.yc + y, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc - x, C.yc + y, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc + x, C.yc - y, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc - x, C.yc - y, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc + y, C.yc + x, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc - y, C.yc + x, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc + y, C.yc - x, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc - y, C.yc - x, C.thickNess, C.thickNess)
        End If
        While x < y
            If d < 0 Then
                d = d + 2 * x + 2
            Else 'd>=0
                d = d + 2 * (x - y) + 1
                y = y - 1
            End If
            x = x + 1
            gBmp.FillEllipse(myBrush, C.xc + x, C.yc + y, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc - x, C.yc + y, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc + x, C.yc - y, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc - x, C.yc - y, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc + y, C.yc + x, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc - y, C.yc + x, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc + y, C.yc - x, C.thickNess, C.thickNess)
            gBmp.FillEllipse(myBrush, C.xc - y, C.yc - x, C.thickNess, C.thickNess)
        End While
    End Sub

    Sub scanLineRegionFillWithStack(P As TColor)
        Dim i, countI As Integer
        Dim spanabove, spanbelow As Boolean
        Dim c0 As Color
        Dim S As TStackPoint
        'Init
        c0 = bmp1.GetPixel(P.X, P.Y)
        countI = 0
        If c0 <> P.col Then
            initStack(S)
            push(S, P.X, P.Y)
            While Not isEmptyStack(S)
                pop(S, P.X, P.Y)
                i = P.X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, P.Y) = c0
                    i = i - 1
                End While
                spanabove = False
                spanbelow = False
                While i < bmp1.Width And P.Y + 1 < bmp1.Height AndAlso bmp1.GetPixel(i, P.Y) = c0
                    bmp1.SetPixel(i, P.Y, P.col)
                    If P.Y < bmp1.Height Then
                        If Not spanabove And bmp1.GetPixel(i, P.Y + 1) = c0 Then
                            push(S, i, P.Y + 1)
                            spanabove = True
                        ElseIf spanabove And bmp1.GetPixel(i, P.Y + 1) <> c0 Then
                            spanabove = False
                        End If
                    End If
                    If P.Y > 0 Then
                        If Not spanbelow And bmp1.GetPixel(i, P.Y - 1) = c0 Then
                            push(S, i, P.Y - 1)
                            spanbelow = True
                        ElseIf spanbelow And bmp1.GetPixel(i, P.Y - 1) <> c0 Then
                            spanbelow = False
                        End If
                    End If
                    i = i + 1
                    countI = countI + 1
                    'pict.Image = bmp1
                End While
            End While
        End If
    End Sub

    Sub strippedFill(P As TColor)
        Dim i, temp As Integer
        Dim spanabove, spanbelow, strip, span As Boolean
        Dim c0 As Color
        Dim S As TStackPoint
        'Init
        temp = 0
        c0 = bmp1.GetPixel(P.X, P.Y)
        temp = P.Y
        strip = True
        span = False
        If c0 <> P.col Then
            initStack(S)
            push(S, P.X, P.Y)
            While Not isEmptyStack(S)
                pop(S, P.X, P.Y)
                i = P.X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, P.Y) = c0
                    i = i - 1
                End While
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, P.Y) = c0
                    If strip Then
                        bmp1.SetPixel(i, P.Y, P.col)
                    End If
                    If P.Y < bmp1.Height Then
                        If Not spanabove And bmp1.GetPixel(i, P.Y + 1) = c0 Then
                            push(S, i, P.Y + 1)
                            spanabove = True
                        ElseIf spanabove And bmp1.GetPixel(i, P.Y + 1) <> c0 Then
                            spanabove = False
                        End If
                    End If
                    i = i + 1
                End While
                strip = Not strip
            End While
            P.Y = temp - 1
            initStack(S)
            push(S, P.X, P.Y)
            While Not isEmptyStack(S)
                pop(S, P.X, P.Y)
                i = P.X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, P.Y) = c0
                    i = i - 1
                End While
                temp = i
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, P.Y) = c0
                    If strip Then
                        bmp1.SetPixel(i, P.Y, P.col)
                    End If
                    If P.Y > 0 Then
                        If Not spanbelow And bmp1.GetPixel(i, P.Y - 1) = c0 Then
                            push(S, i, P.Y - 1)
                            spanbelow = True
                        ElseIf spanbelow And bmp1.GetPixel(i, P.Y - 1) <> c0 Then
                            spanbelow = False
                        End If
                    End If
                    i = i + 1
                End While
                strip = Not strip
            End While
        End If
    End Sub

    Sub checkeredFill(P As TColor, W As Integer, H As Integer)
        Dim i, temp, countW, countH As Integer
        Dim spanabove, spanbelow, checkered, span As Boolean
        Dim c0 As Color
        Dim S As TStackPoint
        'Init
        temp = 0
        c0 = bmp1.GetPixel(P.X, P.Y)
        temp = P.Y
        checkered = True
        span = False
        countW = 1
        countH = 1
        If c0 <> P.col Then
            initStack(S)
            push(S, P.X, P.Y)
            While Not isEmptyStack(S)
                pop(S, P.X, P.Y)
                i = P.X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, P.Y) = c0
                    i = i - 1
                End While
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, P.Y) = c0
                    If checkered Then
                        bmp1.SetPixel(i, P.Y, P.col)
                    End If
                    If P.Y < bmp1.Height Then
                        If Not spanabove And bmp1.GetPixel(i, P.Y + 1) = c0 Then
                            push(S, i, P.Y + 1)
                            spanabove = True
                        ElseIf spanabove And bmp1.GetPixel(i, P.Y + 1) <> c0 Then
                            spanabove = False
                        End If
                    End If
                    'If Y > 0 Then
                    '    If Not spanbelow And bmp1.GetPixel(i, Y - 1) = c0 Then
                    '        push(S, i, Y - 1)
                    '        spanbelow = True
                    '    ElseIf spanbelow And bmp1.GetPixel(i, Y - 1) <> c0 Then
                    '        spanbelow = False
                    '    End If
                    'End If
                    If countW = W Then
                        checkered = Not checkered
                        countW = 1
                    Else 'count <> W
                        countW = countW + 1
                    End If
                    i = i + 1
                End While
                If countH = H Then
                    checkered = Not checkered
                    countH = 1
                Else 'count <> W
                    countH = countH + 1
                End If
            End While
            P.Y = temp - 1
            initStack(S)
            push(S, P.X, P.Y)
            checkered = False
            countH = 1
            countW = 1
            While Not isEmptyStack(S)
                pop(S, P.X, P.Y)
                i = P.X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, P.Y) = c0
                    i = i - 1
                End While
                temp = i
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, P.Y) = c0
                    If checkered Then
                        bmp1.SetPixel(i, P.Y, P.col)
                    End If
                    'If Y < bmp1.Height Then
                    '    If Not spanabove And bmp1.GetPixel(i, Y + 1) = c0 Then
                    '        push(S, i, Y + 1)
                    '        spanabove = True
                    '    ElseIf spanabove And bmp1.GetPixel(i, Y + 1) <> c0 Then
                    '        spanabove = False
                    '    End If
                    'End If
                    If P.Y > 0 Then
                        If Not spanbelow And bmp1.GetPixel(i, P.Y - 1) = c0 Then
                            push(S, i, P.Y - 1)
                            spanbelow = True
                        ElseIf spanbelow And bmp1.GetPixel(i, P.Y - 1) <> c0 Then
                            spanbelow = False
                        End If
                    End If
                    If countW = W Then
                        checkered = Not checkered
                        countW = 1
                    Else 'count <> W
                        countW = countW + 1
                    End If
                    i = i + 1
                End While
                If countH = H Then
                    checkered = Not checkered
                    countH = 1
                Else 'count <> W
                    countH = countH + 1
                End If
            End While
        End If
    End Sub

    Sub SHOWcheckeredFill(X As Integer, Y As Integer, W As Integer, H As Integer, C As Color, pict As PictureBox)
        Dim i, temp, countW, countH As Integer
        Dim spanabove, spanbelow, checkered, span As Boolean
        Dim c0 As Color
        Dim S As TStackPoint
        'Init
        temp = 0
        c0 = bmp1.GetPixel(X, Y)
        temp = Y
        checkered = True
        span = False
        countW = 1
        countH = 1
        If c0 <> C Then
            initStack(S)
            push(S, X, Y)
            While Not isEmptyStack(S)
                pop(S, X, Y)
                i = X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, Y) = c0
                    i = i - 1
                End While
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, Y) = c0
                    If checkered Then
                        bmp1.SetPixel(i, Y, C)
                        pict.Image = bmp1
                        Application.DoEvents()
                        System.Threading.Thread.Sleep(100)
                    End If
                    If Y < bmp1.Height Then
                        If Not spanabove And bmp1.GetPixel(i, Y + 1) = c0 Then
                            push(S, i, Y + 1)
                            spanabove = True
                        ElseIf spanabove And bmp1.GetPixel(i, Y + 1) <> c0 Then
                            spanabove = False
                        End If
                    End If
                    'If Y > 0 Then
                    '    If Not spanbelow And bmp1.GetPixel(i, Y - 1) = c0 Then
                    '        push(S, i, Y - 1)
                    '        spanbelow = True
                    '    ElseIf spanbelow And bmp1.GetPixel(i, Y - 1) <> c0 Then
                    '        spanbelow = False
                    '    End If
                    'End If
                    If countW = W Then
                        checkered = Not checkered
                        countW = 1
                    Else 'count <> W
                        countW = countW + 1
                    End If
                    i = i + 1
                End While
                If countH = H Then
                    checkered = Not checkered
                    countH = 1
                Else 'count <> W
                    countH = countH + 1
                End If
            End While
            Y = temp - 1
            initStack(S)
            push(S, X, Y)
            checkered = False
            countH = 1
            countW = 1
            While Not isEmptyStack(S)
                pop(S, X, Y)
                i = X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, Y) = c0
                    i = i - 1
                End While
                temp = i
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, Y) = c0
                    If checkered Then
                        bmp1.SetPixel(i, Y, C)
                        pict.Image = bmp1
                        Application.DoEvents()
                        System.Threading.Thread.Sleep(100)
                    End If
                    'If Y < bmp1.Height Then
                    '    If Not spanabove And bmp1.GetPixel(i, Y + 1) = c0 Then
                    '        push(S, i, Y + 1)
                    '        spanabove = True
                    '    ElseIf spanabove And bmp1.GetPixel(i, Y + 1) <> c0 Then
                    '        spanabove = False
                    '    End If
                    'End If
                    If Y > 0 Then
                        If Not spanbelow And bmp1.GetPixel(i, Y - 1) = c0 Then
                            push(S, i, Y - 1)
                            spanbelow = True
                        ElseIf spanbelow And bmp1.GetPixel(i, Y - 1) <> c0 Then
                            spanbelow = False
                        End If
                    End If

                    If countW = W Then
                        checkered = Not checkered
                        countW = 1
                    Else 'count <> W
                        countW = countW + 1
                    End If
                    i = i + 1
                End While
                If countH = H Then
                    checkered = Not checkered
                    countH = 1
                Else 'count <> W
                    countH = countH + 1
                End If
            End While
        End If
    End Sub

    Sub scanLineRegionFill(P As TColor)
        Dim c0 As Color
        c0 = bmp1.GetPixel(P.X, P.Y)
        If c0 <> P.col Then
            doit2(P.X, P.Y, P.col, c0)
        End If
    End Sub

    Sub doit2(X As Integer, Y As Integer, C As Color, C0 As Color)
        Dim i, L, R As Integer
        i = X
        overFlow = overFlow + 1
        If overFlow < 461 Then
            If C0 <> C Then
                While i > 0 And Y > 0 AndAlso bmp1.GetPixel(i, Y) = C0
                    bmp1.SetPixel(i, Y, C)
                    i = i - 1
                    If i < 0 Or bmp1.GetPixel(i, Y) <> C0 Then
                        L = i + 1
                    End If
                End While
                i = X + 1
                While i <= bmp1.Width AndAlso bmp1.GetPixel(i, Y) = C0
                    bmp1.SetPixel(i, Y, C)
                    i = i + 1
                    If bmp1.GetPixel(i, Y) <> C0 Then
                        R = i - 1
                    End If
                End While
                For i = L To R
                    If bmp1.GetPixel(i, Y + 1) = C0 Then
                        doit2(i, Y + 1, C, C0)
                    End If
                    If bmp1.GetPixel(i, Y - 1) = C0 Then
                        doit2(i, Y - 1, C, C0)
                    End If
                Next
            End If
        Else 'IsOverFlow
            If notTakingTheload And overFlow = 460 Then
                MsgBox("The system is overflow!")
            End If
        End If
    End Sub

    Sub floodfill(P As TColor)
        Dim c0 As Color
        c0 = bmp1.GetPixel(P.X, P.Y)
        If Not P.col = c0 Then
            doit(P.X, P.Y, P.col, c0)
        End If
    End Sub

    Sub doit(x As Integer, y As Integer, c As Color, c0 As Color)
        bmp1.SetPixel(x, y, c)
        overFlow += 1
        If overFlow < 461 Then
            If x > 0 AndAlso bmp1.GetPixel(x - 1, y) = c0 Then
                doit(x - 1, y, c, c0)
            End If
            If x > 0 And y > 0 AndAlso bmp1.GetPixel(x - 1, y - 1) = c0 Then
                doit(x - 1, y - 1, c, c0)
            End If
            If y > 0 AndAlso bmp1.GetPixel(x, y - 1) = c0 Then
                doit(x, y - 1, c, c0)
            End If
            If x < bmp1.Width And y > 0 AndAlso bmp1.GetPixel(x + 1, y - 1) = c0 Then
                doit(x + 1, y - 1, c, c0)
            End If
            If x < bmp1.Width AndAlso bmp1.GetPixel(x + 1, y) = c0 Then
                doit(x + 1, y, c, c0)
            End If
            If x < bmp1.Width And y < bmp1.Height AndAlso bmp1.GetPixel(x + 1, y + 1) = c0 Then
                doit(x + 1, y + 1, c, c0)
            End If
            If y < bmp1.Height AndAlso bmp1.GetPixel(x, y + 1) = c0 Then
                doit(x, y + 1, c, c0)
            End If
            If x > 0 And y > bmp1.Height AndAlso bmp1.GetPixel(x - 1, y + 1) = c0 Then
                doit(x - 1, y + 1, c, c0)
            End If
        Else 'IsOverFlow
            If notTakingTheload And overFlow = 462 Then
                MsgBox("The system is overflow!")
            End If
        End If
    End Sub

    Sub floodfillwithstack(P As TColor)
        Dim c0 As Color
        Dim s As TStackPoint
        c0 = bmp1.GetPixel(P.X, P.Y)
        If Not P.col = c0 And P.X > 0 Then
            initStack(s)
            push(s, P.X, P.Y)
            While Not isEmptyStack(s)
                pop(s, P.X, P.Y)
                bmp1.SetPixel(P.X, P.Y, P.col)
                If P.X > 0 AndAlso bmp1.GetPixel(P.X - 1, P.Y) = c0 Then
                    push(s, P.X - 1, P.Y)
                End If
                If P.X > 0 And P.Y > 0 AndAlso bmp1.GetPixel(P.X - 1, P.Y - 1) = c0 Then
                    push(s, P.X - 1, P.Y - 1)
                End If
                If P.Y > 0 AndAlso bmp1.GetPixel(P.X, P.Y - 1) = c0 Then
                    push(s, P.X, P.Y - 1)
                End If
                If P.X + 1 < bmp1.Width And P.Y > 0 AndAlso bmp1.GetPixel(P.X + 1, P.Y - 1) = c0 Then
                    push(s, P.X + 1, P.Y - 1)
                End If
                If P.X + 1 < bmp1.Width AndAlso bmp1.GetPixel(P.X + 1, P.Y) = c0 Then
                    push(s, P.X + 1, P.Y)
                End If
                If P.X + 1 < bmp1.Width And P.Y + 1 < bmp1.Height AndAlso bmp1.GetPixel(P.X + 1, P.Y + 1) = c0 Then
                    push(s, P.X + 1, P.Y + 1)
                End If
                If P.Y + 1 < bmp1.Height AndAlso bmp1.GetPixel(P.X, P.Y + 1) = c0 Then
                    push(s, P.X, P.Y + 1)
                End If
                If P.X > 0 And P.Y + 1 > bmp1.Height AndAlso bmp1.GetPixel(P.X - 1, P.Y + 1) = c0 Then
                    push(s, P.X - 1, P.Y + 1)
                End If
            End While
        End If
    End Sub

    Sub scanLineBoundaryFill(P As TColor)
        If bmp1.GetPixel(P.X, P.Y) <> P.bOund Then
            doit4(P.X, P.Y, P.bOund, P.col)
        End If
    End Sub

    Sub scanLineBoundaryFillWithStack(P As TColor)
        Dim i As Integer
        Dim spanabove, spanbelow As Boolean
        Dim S As TStackPoint
        If bmp1.GetPixel(P.X, P.Y) <> P.bOund Then
            'Init
            initStack(S)
            push(S, P.X, P.Y)
            While Not isEmptyStack(S)
                pop(S, P.X, P.Y)
                i = P.X
                While i - 1 > 0 AndAlso bmp1.GetPixel(i - 1, P.Y) <> P.bOund And bmp1.GetPixel(i - 1, P.Y) <> P.col
                    i = i - 1
                End While
                spanabove = False
                spanbelow = False
                While i + 1 < bmp1.Width And P.Y + 1 < bmp1.Height AndAlso bmp1.GetPixel(i, P.Y) <> P.bOund And bmp1.GetPixel(i, P.Y) <> P.col
                    bmp1.SetPixel(i, P.Y, P.col)
                    If P.Y < bmp1.Height Then
                        If Not spanabove And bmp1.GetPixel(i, P.Y + 1) <> P.bOund And bmp1.GetPixel(i, P.Y + 1) <> P.col Then
                            push(S, i, P.Y + 1)
                            spanabove = True
                        ElseIf spanabove And bmp1.GetPixel(i, P.Y + 1) = P.bOund Or bmp1.GetPixel(i, P.Y + 1) = P.col Then
                            spanabove = False
                        End If
                    End If
                    If P.Y > 0 Then
                        If Not spanbelow And bmp1.GetPixel(i, P.Y - 1) <> P.bOund And bmp1.GetPixel(i, P.Y - 1) <> P.col Then
                            push(S, i, P.Y - 1)
                            spanbelow = True
                        ElseIf spanbelow And bmp1.GetPixel(i, P.Y - 1) = P.bOund Or bmp1.GetPixel(i, P.Y - 1) = P.col Then
                            spanbelow = False
                        End If
                    End If
                    i = i + 1
                End While
            End While
        End If
    End Sub

    Sub doit4(X As Integer, Y As Integer, B As Color, C As Color)
        Dim i, L, R As Integer
        i = X
        overFlow += 1
        If overFlow < 461 Then
            If bmp1.GetPixel(X, Y) <> B Then
                While i >= 0 AndAlso bmp1.GetPixel(i, Y) <> C And bmp1.GetPixel(i, Y) <> B
                    bmp1.SetPixel(i, Y, C)
                    i = i - 1
                    If i < 0 Or bmp1.GetPixel(i, Y) = B Or bmp1.GetPixel(i, Y) = C Then
                        L = i + 1
                    End If
                End While
                i = X + 1
                While i <= bmp1.Width AndAlso bmp1.GetPixel(i, Y) <> C And bmp1.GetPixel(i, Y) <> B
                    bmp1.SetPixel(i, Y, C)
                    i = i + 1
                    If bmp1.GetPixel(i, Y) = C Or bmp1.GetPixel(i, Y) = B Then
                        R = i - 1
                    End If
                End While
                For i = L To R
                    If bmp1.GetPixel(i, Y + 1) <> C And bmp1.GetPixel(i, Y + 1) <> B Then
                        doit4(i, Y + 1, B, C)
                    End If
                    If bmp1.GetPixel(i, Y - 1) <> C And bmp1.GetPixel(i, Y - 1) <> B Then
                        doit4(i, Y - 1, B, C)
                    End If
                Next
            Else 'IsOverFlow
                If notTakingTheload And overFlow = 462 Then
                    MsgBox("The system is overflow!")
                End If
            End If

        End If
    End Sub

    Sub boundaryFillWithStack(P As TColor)
        Dim s As TStackPoint
        If bmp1.GetPixel(P.X, P.Y) <> P.bOund Then
            initStack(s)
            push(s, P.X, P.Y)
            While Not isEmptyStack(s)
                pop(s, P.X, P.Y)
                bmp1.SetPixel(P.X, P.Y, P.col)
                If P.X > 0 AndAlso bmp1.GetPixel(P.X - 1, P.Y) <> P.bOund And bmp1.GetPixel(P.X - 1, P.Y) <> P.col Then
                    push(s, P.X - 1, P.Y)
                End If
                If P.X > 0 And P.Y > 0 AndAlso bmp1.GetPixel(P.X - 1, P.Y - 1) <> P.bOund And bmp1.GetPixel(P.X - 1, P.Y - 1) <> P.col Then
                    push(s, P.X - 1, P.Y - 1)
                End If
                If P.Y > 0 AndAlso bmp1.GetPixel(P.X, P.Y - 1) <> P.bOund And bmp1.GetPixel(P.X, P.Y - 1) <> P.col Then
                    push(s, P.X, P.Y - 1)
                End If
                If P.X < bmp1.Width And P.Y > 0 AndAlso bmp1.GetPixel(P.X + 1, P.Y - 1) <> P.bOund And bmp1.GetPixel(P.X + 1, P.Y - 1) <> P.col Then
                    push(s, P.X + 1, P.Y - 1)
                End If
                If P.X < bmp1.Width AndAlso bmp1.GetPixel(P.X + 1, P.Y) <> P.bOund And bmp1.GetPixel(P.X + 1, P.Y) <> P.col Then
                    push(s, P.X + 1, P.Y)
                End If
                If P.X < bmp1.Width And P.Y < bmp1.Height AndAlso bmp1.GetPixel(P.X + 1, P.Y + 1) <> P.bOund And bmp1.GetPixel(P.X + 1, P.Y + 1) <> P.col Then
                    push(s, P.X + 1, P.Y + 1)
                End If
                If P.Y < bmp1.Height AndAlso bmp1.GetPixel(P.X, P.Y + 1) <> P.bOund And bmp1.GetPixel(P.X, P.Y + 1) <> P.col Then
                    push(s, P.X, P.Y + 1)
                End If
                If P.X > 0 And P.Y > bmp1.Height AndAlso bmp1.GetPixel(P.X - 1, P.Y + 1) <> P.bOund And bmp1.GetPixel(P.X - 1, P.Y + 1) <> P.col Then
                    push(s, P.X - 1, P.Y + 1)
                End If
            End While
        End If
    End Sub

    Sub boundaryFill(P As TColor)
        If bmp1.GetPixel(P.X, P.Y) <> P.bOund Then
            DoIt3(P.X, P.Y, P.bOund, P.col)
        End If
    End Sub

    Sub DoIt3(X As Integer, Y As Integer, B As Color, C As Color)
        bmp1.SetPixel(X, Y, C)
        If overFlow < 461 Then
            If bmp1.GetPixel(X - 1, Y) <> B And bmp1.GetPixel(X - 1, Y) <> C Then
                DoIt3(X - 1, Y, B, C)
            End If
            If bmp1.GetPixel(X + 1, Y) <> B And bmp1.GetPixel(X + 1, Y) <> C Then
                DoIt3(X + 1, Y, B, C)
            End If
            If bmp1.GetPixel(X, Y - 1) <> B And bmp1.GetPixel(X, Y - 1) <> C Then
                DoIt3(X, Y - 1, B, C)
            End If
            If bmp1.GetPixel(X, Y + 1) <> B And bmp1.GetPixel(X, Y + 1) <> C Then
                DoIt3(X, Y + 1, B, C)
            End If
        Else 'IsOverFlow
            If notTakingTheload And overFlow = 462 Then
                MsgBox("The system is overflow!")
            End If
        End If
    End Sub

    Sub SHOWscanLineRegionFill(X As Integer, Y As Integer, C As Color, pict As PictureBox)
        Dim c0 As Color
        c0 = bmp1.GetPixel(X, Y)
        If c0 <> C Then
            SHOWdoit2(X, Y, C, c0, pict)
        End If
    End Sub

    Sub SHOWdoit2(X As Integer, Y As Integer, C As Color, C0 As Color, pict As PictureBox)
        Dim i, L, R As Integer
        i = X
        overFlow += 1
        If overFlow < 461 Then
            If C0 <> C Then
                While i >= 0 AndAlso bmp1.GetPixel(i, Y) = C0
                    bmp1.SetPixel(i, Y, C)
                    pict.Image = bmp1
                    Application.DoEvents()
                    System.Threading.Thread.Sleep(100)
                    i = i - 1
                    If i < 0 Or bmp1.GetPixel(i, Y) <> C0 Then
                        L = i + 1
                    End If
                End While
                i = X + 1
                While i <= bmp1.Width AndAlso bmp1.GetPixel(i, Y) = C0
                    bmp1.SetPixel(i, Y, C)
                    pict.Image = bmp1
                    Application.DoEvents()
                    System.Threading.Thread.Sleep(100)
                    i = i + 1
                    If bmp1.GetPixel(i, Y) <> C0 Then
                        R = i - 1
                    End If
                End While
                For i = L To R
                    If bmp1.GetPixel(i, Y + 1) = C0 Then
                        SHOWdoit2(i, Y + 1, C, C0, pict)
                    End If
                    If bmp1.GetPixel(i, Y - 1) = C0 Then
                        SHOWdoit2(i, Y - 1, C, C0, pict)
                    End If
                Next
            Else 'IsOverFlow
                If notTakingTheload And overFlow = 462 Then
                    MsgBox("The system is overflow!")
                End If
            End If
        End If
    End Sub

    Sub SHOWfloodfill(x As Integer, y As Integer, c As Color, pict As PictureBox)
        Dim c0 As Color
        c0 = bmp1.GetPixel(x, y)
        If Not c = c0 Then
            SHOWdoit(x, y, c, c0, pict)
        End If
    End Sub

    Sub SHOWdoit(x As Integer, y As Integer, c As Color, c0 As Color, pict As PictureBox)
        bmp1.SetPixel(x, y, c)
        pict.Image = bmp1
        Application.DoEvents()
        System.Threading.Thread.Sleep(100)
        overFlow += 1
        If overFlow < 461 Then
            If x > 0 AndAlso bmp1.GetPixel(x - 1, y) = c0 Then
                SHOWdoit(x - 1, y, c, c0, pict)
            End If
            If x > 0 And y > 0 AndAlso bmp1.GetPixel(x - 1, y - 1) = c0 Then
                SHOWdoit(x - 1, y - 1, c, c0, pict)
            End If
            If y > 0 AndAlso bmp1.GetPixel(x, y - 1) = c0 Then
                SHOWdoit(x, y - 1, c, c0, pict)
            End If
            If x < bmp1.Width And y > 0 AndAlso bmp1.GetPixel(x + 1, y - 1) = c0 Then
                SHOWdoit(x + 1, y - 1, c, c0, pict)
            End If
            If x < bmp1.Width AndAlso bmp1.GetPixel(x + 1, y) = c0 Then
                SHOWdoit(x + 1, y, c, c0, pict)
            End If
            If x < bmp1.Width And y < bmp1.Height AndAlso bmp1.GetPixel(x + 1, y + 1) = c0 Then
                SHOWdoit(x + 1, y + 1, c, c0, pict)
            End If
            If y < bmp1.Height AndAlso bmp1.GetPixel(x, y + 1) = c0 Then
                SHOWdoit(x, y + 1, c, c0, pict)
            End If
            If x > 0 And y > bmp1.Height AndAlso bmp1.GetPixel(x - 1, y + 1) = c0 Then
                SHOWdoit(x - 1, y + 1, c, c0, pict)
            End If
        Else 'IsOverFlow
            If notTakingTheload And overFlow = 462 Then
                MsgBox("The system is overflow!")
            End If
        End If
    End Sub

    Sub SHOWfloodfillwithstack(x As Integer, y As Integer, c As Color, pict As PictureBox)
        Dim c0 As Color
        Dim s As TStackPoint
        c0 = bmp1.GetPixel(x, y)
        If Not c = c0 Then
            initStack(s)
            push(s, x, y)
            While Not isEmptyStack(s)
                pop(s, x, y)
                bmp1.SetPixel(x, y, c)
                pict.Image = bmp1
                Application.DoEvents()
                System.Threading.Thread.Sleep(100)
                If x > 0 AndAlso bmp1.GetPixel(x - 1, y) = c0 Then
                    push(s, x - 1, y)
                End If
                If x > 0 And y > 0 AndAlso bmp1.GetPixel(x - 1, y - 1) = c0 Then
                    push(s, x - 1, y - 1)
                End If
                If y > 0 AndAlso bmp1.GetPixel(x, y - 1) = c0 Then
                    push(s, x, y - 1)
                End If
                If x < bmp1.Width And y > 0 AndAlso bmp1.GetPixel(x + 1, y - 1) = c0 Then
                    push(s, x + 1, y - 1)
                End If
                If x < bmp1.Width AndAlso bmp1.GetPixel(x + 1, y) = c0 Then
                    push(s, x + 1, y)
                End If
                If x < bmp1.Width And y < bmp1.Height AndAlso bmp1.GetPixel(x + 1, y + 1) = c0 Then
                    push(s, x + 1, y + 1)
                End If
                If y < bmp1.Height AndAlso bmp1.GetPixel(x, y + 1) = c0 Then
                    push(s, x, y + 1)
                End If
                If x > 0 And y > bmp1.Height AndAlso bmp1.GetPixel(x - 1, y + 1) = c0 Then
                    push(s, x - 1, y + 1)
                End If
            End While
        End If
    End Sub

    Sub SHOWscanLineBoundaryFill(X As Integer, Y As Integer, B As Color, C As Color, pict As PictureBox)
        If bmp1.GetPixel(X, Y) <> B Then
            SHOWdoit4(X, Y, B, C, pict)
        End If
    End Sub

    Sub SHOWscanLineBoundaryFillWithStack(X As Integer, Y As Integer, B As Color, C As Color, pict As PictureBox)
        Dim i As Integer
        Dim spanabove, spanbelow As Boolean
        Dim S As TStackPoint

        If bmp1.GetPixel(X, Y) <> B Then
            'Init
            initStack(S)
            push(S, X, Y)
            While Not isEmptyStack(S)
                pop(S, X, Y)
                i = X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, Y) <> B And bmp1.GetPixel(i - 1, Y) <> C
                    i = i - 1
                End While
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, Y) <> B And bmp1.GetPixel(i, Y) <> C
                    bmp1.SetPixel(i, Y, C)
                    pict.Image = bmp1
                    Application.DoEvents()
                    System.Threading.Thread.Sleep(100)
                    If Y < bmp1.Height Then
                        If Not spanabove And bmp1.GetPixel(i, Y + 1) <> B And bmp1.GetPixel(i, Y + 1) <> C Then
                            push(S, i, Y + 1)
                            spanabove = True
                        ElseIf spanabove And bmp1.GetPixel(i, Y + 1) = B Or bmp1.GetPixel(i, Y + 1) = C Then
                            spanabove = False
                        End If
                    End If
                    If Y > 0 Then
                        If Not spanbelow And bmp1.GetPixel(i, Y - 1) <> B And bmp1.GetPixel(i, Y - 1) <> C Then
                            push(S, i, Y - 1)
                            spanbelow = True
                        ElseIf spanbelow And bmp1.GetPixel(i, Y - 1) = B Or bmp1.GetPixel(i, Y - 1) = C Then
                            spanbelow = False
                        End If
                    End If
                    i = i + 1
                End While
            End While
        End If
    End Sub

    Sub SHOWdoit4(X As Integer, Y As Integer, B As Color, C As Color, pict As PictureBox)
        Dim i, L, R As Integer
        i = X
        overFlow += 1
        If overFlow < 461 Then
            If bmp1.GetPixel(X, Y) <> B Then
                While i >= 0 AndAlso bmp1.GetPixel(i, Y) <> C And bmp1.GetPixel(i, Y) <> B
                    bmp1.SetPixel(i, Y, C)
                    pict.Image = bmp1
                    Application.DoEvents()
                    System.Threading.Thread.Sleep(100)
                    i = i - 1
                    If i < 0 Or bmp1.GetPixel(i, Y) = B Or bmp1.GetPixel(i, Y) = C Then
                        L = i + 1
                    End If
                End While
                i = X + 1
                While i <= bmp1.Width AndAlso bmp1.GetPixel(i, Y) <> C And bmp1.GetPixel(i, Y) <> B
                    bmp1.SetPixel(i, Y, C)
                    pict.Image = bmp1
                    Application.DoEvents()
                    System.Threading.Thread.Sleep(100)
                    i = i + 1
                    If bmp1.GetPixel(i, Y) = C Or bmp1.GetPixel(i, Y) = B Then
                        R = i - 1
                    End If
                End While
                For i = L To R
                    If bmp1.GetPixel(i, Y + 1) <> C And bmp1.GetPixel(i, Y + 1) <> B Then
                        SHOWdoit4(i, Y + 1, B, C, pict)
                    End If
                    If bmp1.GetPixel(i, Y - 1) <> C And bmp1.GetPixel(i, Y - 1) <> B Then
                        SHOWdoit4(i, Y - 1, B, C, pict)
                    End If
                Next
            Else 'IsOverFlow
                If notTakingTheload And overFlow = 462 Then
                    MsgBox("The system is overflow!")
                End If
            End If
        End If
    End Sub

    Sub SHOWboundaryFillWithStack(X As Integer, Y As Integer, B As Color, C As Color, pict As PictureBox)
        Dim s As TStackPoint
        If bmp1.GetPixel(X, Y) <> B Then
            initStack(s)
            push(s, X, Y)
            While Not isEmptyStack(s)
                pop(s, X, Y)
                bmp1.SetPixel(X, Y, C)
                pict.Image = bmp1
                Application.DoEvents()
                System.Threading.Thread.Sleep(100)
                If X > 0 AndAlso bmp1.GetPixel(X - 1, Y) <> B And bmp1.GetPixel(X - 1, Y) <> C Then
                    push(s, X - 1, Y)
                End If
                If X > 0 And Y > 0 AndAlso bmp1.GetPixel(X - 1, Y - 1) <> B And bmp1.GetPixel(X - 1, Y - 1) <> C Then
                    push(s, X - 1, Y - 1)
                End If
                If Y > 0 AndAlso bmp1.GetPixel(X, Y - 1) <> B And bmp1.GetPixel(X, Y - 1) <> C Then
                    push(s, X, Y - 1)
                End If
                If X < bmp1.Width And Y > 0 AndAlso bmp1.GetPixel(X + 1, Y - 1) <> B And bmp1.GetPixel(X + 1, Y - 1) <> C Then
                    push(s, X + 1, Y - 1)
                End If
                If X < bmp1.Width AndAlso bmp1.GetPixel(X + 1, Y) <> B And bmp1.GetPixel(X + 1, Y) <> C Then
                    push(s, X + 1, Y)
                End If
                If X < bmp1.Width And Y < bmp1.Height AndAlso bmp1.GetPixel(X + 1, Y + 1) <> B And bmp1.GetPixel(X + 1, Y + 1) <> C Then
                    push(s, X + 1, Y + 1)
                End If
                If Y < bmp1.Height AndAlso bmp1.GetPixel(X, Y + 1) <> B And bmp1.GetPixel(X, Y + 1) <> C Then
                    push(s, X, Y + 1)
                End If
                If X > 0 And Y > bmp1.Height AndAlso bmp1.GetPixel(X - 1, Y + 1) <> B And bmp1.GetPixel(X - 1, Y + 1) <> C Then
                    push(s, X - 1, Y + 1)
                End If
                pict.Image = bmp1
            End While
        End If
    End Sub

    Sub SHOWboundaryFill(X As Integer, Y As Integer, B As Color, C As Color, pict As PictureBox)
        If bmp1.GetPixel(X, Y) <> B Then
            SHOWDoIt3(X, Y, B, C, pict)
        End If
    End Sub

    Sub SHOWDoIt3(X As Integer, Y As Integer, B As Color, C As Color, pict As PictureBox)
        bmp1.SetPixel(X, Y, C)
        pict.Image = bmp1
        Application.DoEvents()
        System.Threading.Thread.Sleep(100)
        overFlow += 1
        If overFlow < 461 Then
            If bmp1.GetPixel(X - 1, Y) <> B And bmp1.GetPixel(X - 1, Y) <> C Then
                SHOWDoIt3(X - 1, Y, B, C, pict)
            End If
            If bmp1.GetPixel(X + 1, Y) <> B And bmp1.GetPixel(X + 1, Y) <> C Then
                SHOWDoIt3(X + 1, Y, B, C, pict)
            End If
            If bmp1.GetPixel(X, Y - 1) <> B And bmp1.GetPixel(X, Y - 1) <> C Then
                SHOWDoIt3(X, Y - 1, B, C, pict)
            End If
            If bmp1.GetPixel(X, Y + 1) <> B And bmp1.GetPixel(X, Y + 1) <> C Then
                SHOWDoIt3(X, Y + 1, B, C, pict)
            End If
        Else 'IsOverFlow
            If notTakingTheload And overFlow = 462 Then
                MsgBox("The system is overflow!")
            End If
        End If
    End Sub
    Sub SHOWstrippedFill(X As Integer, Y As Integer, C As Color, pict As PictureBox)
        Dim i, temp As Integer
        Dim spanabove, spanbelow, strip, span As Boolean
        Dim c0 As Color
        Dim S As TStackPoint
        'Init
        temp = 0
        c0 = bmp1.GetPixel(X, Y)
        temp = Y
        strip = True
        span = False
        If c0 <> C Then
            initStack(S)
            push(S, X, Y)
            While Not isEmptyStack(S)
                pop(S, X, Y)
                i = X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, Y) = c0
                    i = i - 1
                End While
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, Y) = c0
                    If strip Then
                        bmp1.SetPixel(i, Y, C)
                        pict.Image = bmp1
                        Application.DoEvents()
                        System.Threading.Thread.Sleep(100)
                    End If
                    If Y < bmp1.Height Then
                        If Not spanabove And bmp1.GetPixel(i, Y + 1) = c0 Then
                            push(S, i, Y + 1)
                            spanabove = True
                        ElseIf spanabove And bmp1.GetPixel(i, Y + 1) <> c0 Then
                            spanabove = False
                        End If
                    End If
                    i = i + 1
                End While
                strip = Not strip
            End While
            Y = temp - 1
            initStack(S)
            push(S, X, Y)
            While Not isEmptyStack(S)
                pop(S, X, Y)
                i = X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, Y) = c0
                    i = i - 1
                End While
                temp = i
                spanabove = False
                spanbelow = False
                While i < bmp1.Width AndAlso bmp1.GetPixel(i, Y) = c0
                    If strip Then
                        bmp1.SetPixel(i, Y, C)
                        pict.Image = bmp1
                        Application.DoEvents()
                        System.Threading.Thread.Sleep(100)
                    End If
                    If Y > 0 Then
                        If Not spanbelow And bmp1.GetPixel(i, Y - 1) = c0 Then
                            push(S, i, Y - 1)
                            spanbelow = True
                        ElseIf spanbelow And bmp1.GetPixel(i, Y - 1) <> c0 Then
                            spanbelow = False
                        End If
                    End If
                    i = i + 1
                End While
                strip = Not strip
            End While
        End If
    End Sub
    Sub SHOWscanLineRegionFillWithStack(P As TColor, pict As PictureBox)
        Dim i, countI As Integer
        Dim spanabove, spanbelow As Boolean
        Dim c0 As Color
        Dim S As TStackPoint
        'Init
        c0 = bmp1.GetPixel(P.X, P.Y)
        countI = 0
        If c0 <> P.col Then
            initStack(S)
            push(S, P.X, P.Y)
            While Not isEmptyStack(S)
                pop(S, P.X, P.Y)
                i = P.X
                While i > 0 AndAlso bmp1.GetPixel(i - 1, P.Y) = c0
                    i = i - 1
                End While
                spanabove = False
                spanbelow = False
                While i < bmp1.Width And P.Y + 1 < bmp1.Height AndAlso bmp1.GetPixel(i, P.Y) = c0
                    bmp1.SetPixel(i, P.Y, P.col)
                    pict.Image = bmp1
                    Application.DoEvents()
                    System.Threading.Thread.Sleep(100)
                    If P.Y < bmp1.Height Then
                        If Not spanabove And bmp1.GetPixel(i, P.Y + 1) = c0 Then
                            push(S, i, P.Y + 1)
                            spanabove = True
                        ElseIf spanabove And bmp1.GetPixel(i, P.Y + 1) <> c0 Then
                            spanabove = False
                        End If
                    End If
                    If P.Y > 0 Then
                        If Not spanbelow And bmp1.GetPixel(i, P.Y - 1) = c0 Then
                            push(S, i, P.Y - 1)
                            spanbelow = True
                        ElseIf spanbelow And bmp1.GetPixel(i, P.Y - 1) <> c0 Then
                            spanbelow = False
                        End If
                    End If
                    i = i + 1
                    countI = countI + 1
                    'pict.Image = bmp1
                End While
            End While
        End If
    End Sub
End Module
