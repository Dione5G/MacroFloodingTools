VERSION 5.00
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H00400040&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   " © Dione5G - Macro Flooding Tools"
   ClientHeight    =   3150
   ClientLeft      =   1185
   ClientTop       =   1260
   ClientWidth     =   4710
   ClipControls    =   0   'False
   DrawMode        =   1  'Blackness
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   2.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   FontTransparent =   0   'False
   HasDC           =   0   'False
   Icon            =   "Form1.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MousePointer    =   99  'Custom
   NegotiateMenus  =   0   'False
   PaletteMode     =   2  'Custom
   Picture         =   "Form1.frx":57E2
   ScaleHeight     =   5.556
   ScaleMode       =   0  'User
   ScaleWidth      =   8.307
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   10
      Left            =   240
      MaxLength       =   100
      TabIndex        =   45
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   2760
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   9
      Left            =   240
      MaxLength       =   100
      TabIndex        =   44
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   2520
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   8
      Left            =   240
      MaxLength       =   100
      TabIndex        =   43
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   2280
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   7
      Left            =   240
      MaxLength       =   100
      TabIndex        =   42
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   2040
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   6
      Left            =   240
      MaxLength       =   100
      TabIndex        =   41
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   1800
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   5
      Left            =   240
      MaxLength       =   100
      TabIndex        =   40
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   1560
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   4
      Left            =   240
      MaxLength       =   100
      TabIndex        =   39
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   1320
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   3
      Left            =   240
      MaxLength       =   100
      TabIndex        =   38
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   1080
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   2
      Left            =   240
      MaxLength       =   100
      TabIndex        =   37
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   840
      Width           =   3255
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   1
      Left            =   240
      MaxLength       =   100
      TabIndex        =   36
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   600
      Width           =   3255
   End
   Begin VB.Timer Timer2 
      Interval        =   100
      Left            =   6360
      Top             =   3360
   End
   Begin VB.Timer Timer1 
      Interval        =   120
      Left            =   0
      Top             =   0
   End
   Begin VB.TextBox c 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Index           =   0
      Left            =   240
      MaxLength       =   100
      TabIndex        =   0
      Text            =   "¡¡       Write text here       !!"
      ToolTipText     =   "Para Vaciar esta caja hagale DobleClic"
      Top             =   360
      Width           =   3255
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   10
      Left            =   6120
      TabIndex        =   35
      Top             =   2760
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   9
      Left            =   6120
      TabIndex        =   34
      Top             =   2520
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   8
      Left            =   6120
      TabIndex        =   33
      Top             =   2280
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   7
      Left            =   6120
      TabIndex        =   32
      Top             =   2040
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   6
      Left            =   6120
      TabIndex        =   31
      Top             =   1800
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   5
      Left            =   6120
      TabIndex        =   30
      Top             =   1560
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   4
      Left            =   6120
      TabIndex        =   29
      Top             =   1320
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   3
      Left            =   6120
      TabIndex        =   28
      Top             =   1080
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   2
      Left            =   6120
      TabIndex        =   27
      Top             =   840
      Width           =   1215
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   1
      Left            =   6120
      TabIndex        =   26
      Top             =   600
      Width           =   1215
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   10
      Left            =   5040
      TabIndex        =   25
      Top             =   2760
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   9
      Left            =   5040
      TabIndex        =   24
      Top             =   2520
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   8
      Left            =   5040
      TabIndex        =   23
      Top             =   2280
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   7
      Left            =   5040
      TabIndex        =   22
      Top             =   2040
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   6
      Left            =   5040
      TabIndex        =   21
      Top             =   1800
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   5
      Left            =   5040
      TabIndex        =   20
      Top             =   1560
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   4
      Left            =   5040
      TabIndex        =   19
      Top             =   1320
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   3
      Left            =   5040
      TabIndex        =   18
      Top             =   1080
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   2
      Left            =   5040
      TabIndex        =   17
      Top             =   840
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   1
      Left            =   5040
      TabIndex        =   16
      Top             =   600
      Width           =   855
   End
   Begin VB.Label filtro 
      BackStyle       =   0  'Transparent
      Caption         =   "filtro"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   255
      Index           =   0
      Left            =   5040
      TabIndex        =   15
      Top             =   360
      Width           =   855
   End
   Begin VB.Label buffer 
      BackStyle       =   0  'Transparent
      Caption         =   "send"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   255
      Index           =   0
      Left            =   6120
      TabIndex        =   14
      Top             =   360
      Width           =   1215
   End
   Begin VB.Label Label14 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F12 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   13
      Top             =   2760
      Width           =   975
   End
   Begin VB.Label Label13 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F11 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   12
      Top             =   2520
      Width           =   975
   End
   Begin VB.Label Label12 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F10 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   11
      Top             =   2280
      Width           =   975
   End
   Begin VB.Label Label11 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F9 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   10
      Top             =   2040
      Width           =   735
   End
   Begin VB.Label Label10 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F8 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   9
      Top             =   1800
      Width           =   735
   End
   Begin VB.Label Label9 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F7 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   8
      Top             =   1560
      Width           =   735
   End
   Begin VB.Label Label8 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F6 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   7
      Top             =   1320
      Width           =   735
   End
   Begin VB.Label Label7 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F5 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   6
      Top             =   1080
      Width           =   735
   End
   Begin VB.Label Label6 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F4 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   5
      Top             =   840
      Width           =   735
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F3 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   4
      Top             =   600
      Width           =   735
   End
   Begin VB.Label Label4 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "F2 key"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   255
      Left            =   3720
      TabIndex        =   3
      Top             =   360
      Width           =   735
   End
   Begin VB.Label Label3 
      BackStyle       =   0  'Transparent
      Caption         =   "Dione5G"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800080&
      Height          =   255
      Left            =   3720
      MousePointer    =   13  'Arrow and Hourglass
      TabIndex        =   2
      ToolTipText     =   "Dione5G"
      Top             =   0
      Width           =   855
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Macro Flooding Tool"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   375
      Left            =   840
      TabIndex        =   1
      ToolTipText     =   "Presiona F1 para más Información"
      Top             =   0
      Width           =   2535
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hWnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long
Private Declare Function GetKeyPress Lib "user32" Alias "GetAsyncKeyState" (ByVal keys As Long) As Integer
Private Sub Form_Load()
On Error Resume Next
MsgBox " MFT Open for use ", "0", "© Dione5G MFT: Running..."
End Sub
Private Sub c_DblClick(Index As Integer)
c(Index).Text = ""
End Sub

Private Sub Label3_Click()
ShellExecute Me.hWnd, "open", "https://www.youtube.com/channel/UC7L97ZkpVRVceO-sqE9NhJw", "", "C:\\", SW_SHOWNORMAL
ShellExecute Me.hWnd, "open", "https://twitter.com/Dionee3G", "", "C:\\", SW_SHOWNORMAL
End Sub

Private Sub Timer1_Timer()
If GetKeyPress(vbKeyF1) Then
On Error Resume Next
Form2.Show
F1:
End If
If GetKeyPress(vbKeyF1) Then
GoTo F1
End If
If GetKeyPress(vbKeyF2) Then
On Error Resume Next
SendKeys (buffer(0).Caption) + ("{enter}")
F2:
End If
If GetKeyPress(vbKeyF2) Then
GoTo F2
End If
If GetKeyPress(vbKeyF3) Then
On Error Resume Next
SendKeys (buffer(1).Caption) + ("{enter}")
F3:
End If
If GetKeyPress(vbKeyF3) Then
GoTo F3
End If
If GetKeyPress(vbKeyF4) Then
On Error Resume Next
SendKeys (buffer(2).Caption) + ("{enter}")
F4:
End If
If GetKeyPress(vbKeyF4) Then
GoTo F4
End If
If GetKeyPress(vbKeyF5) Then
On Error Resume Next
SendKeys (buffer(3).Caption) + ("{enter}")
F5:
End If
If GetKeyPress(vbKeyF5) Then
GoTo F5
End If
If GetKeyPress(vbKeyF6) Then
On Error Resume Next
SendKeys (buffer(4).Caption) + ("{enter}")
F6:
End If
If GetKeyPress(vbKeyF6) Then
GoTo F6
End If
If GetKeyPress(vbKeyF7) Then
On Error Resume Next
SendKeys (buffer(5).Caption) + ("{enter}")
F7:
End If
If GetKeyPress(vbKeyF7) Then
GoTo F7
End If
If GetKeyPress(vbKeyF8) Then
On Error Resume Next
SendKeys (buffer(6).Caption) + ("{enter}")
F8:
End If
If GetKeyPress(vbKeyF8) Then
GoTo F8
End If
If GetKeyPress(vbKeyF9) Then
On Error Resume Next
SendKeys (buffer(7).Caption) + ("{enter}")
F9:
End If
If GetKeyPress(vbKeyF9) Then
GoTo F9
End If
If GetKeyPress(vbKeyF10) Then
On Error Resume Next
SendKeys (buffer(8).Caption) + ("{enter}")
F10:
End If
If GetKeyPress(vbKeyF10) Then
GoTo F10
End If
If GetKeyPress(vbKeyF11) Then
On Error Resume Next
SendKeys (buffer(9).Caption) + ("{enter}")
F11:
End If
If GetKeyPress(vbKeyF11) Then
GoTo F11
End If
If GetKeyPress(vbKeyF12) Then
On Error Resume Next
SendKeys (buffer(10).Caption) + ("{enter}")
F12:
End If
If GetKeyPress(vbKeyF12) Then
GoTo F12
End If
End Sub
Private Sub Timer2_Timer()
For x = 0 To 10
If InStr(c(x).Text, "{") Then
bo1 = Replace(c(x).Text, "{", "°coder1°coder1°")
filtro(x).Caption = bo1
Else
bo1 = c(x).Text
filtro(x).Caption = bo1
End If

If InStr(bo1, "}") Then
bo2 = Replace(bo1, "}", "°coder2°coder2°")
filtro(x).Caption = bo2
Else
bo2 = bo1
filtro(x).Caption = bo2
End If

If InStr(bo2, "(") Then
bo3 = Replace(bo2, "(", "°coder3°coder3°")
filtro(x).Caption = bo3
Else
bo3 = bo2
filtro(x).Caption = bo3
End If

If InStr(bo3, ")") Then
bo4 = Replace(bo3, ")", "°coder4°coder4°")
filtro(x).Caption = bo4
Else
bo4 = bo3
filtro(x).Caption = bo4
End If
If InStr(bo4, "+") Then
bo5 = Replace(bo4, "+", "°coder5°coder5°")
filtro(x).Caption = bo5
Else
bo5 = bo4
filtro(x).Caption = bo5
End If
If InStr(bo5, "^") Then
bo6 = Replace(bo5, "^", "°coder6°coder6°")
filtro(x).Caption = bo6
Else
bo6 = bo5
filtro(x).Caption = bo6
End If
If InStr(bo6, "%") Then
bo7 = Replace(bo6, "%", "°coder7°coder7°")
filtro(x).Caption = bo7
Else
bo7 = bo6
filtro(x).Caption = bo7
End If
If InStr(bo7, "~") Then
bo8 = Replace(bo7, "~", "°coder8°coder8°")
filtro(x).Caption = bo8
Else
bo8 = bo7
filtro(x).Caption = bo8
End If
If InStr(filtro(x).Caption, "°coder1°coder1°") Then
to1 = Replace(filtro(x).Caption, "°coder1°coder1°", "{{}")
buffer(x).Caption = to1
Else
to1 = filtro(x).Caption
buffer(x).Caption = to1
End If

If InStr(to1, "°coder2°coder2°") Then
to2 = Replace(to1, "°coder2°coder2°", "{}}")
buffer(x).Caption = to2
Else
to2 = to1
buffer(x).Caption = to2
End If

If InStr(to2, "°coder3°coder3°") Then
to3 = Replace(to2, "°coder3°coder3°", "{(}")
buffer(x).Caption = to3
Else
to3 = to2
buffer(x).Caption = to3
End If

If InStr(to3, "°coder4°coder4°") Then
to4 = Replace(to3, "°coder4°coder4°", "{)}")
buffer(x).Caption = to4
Else
to4 = to3
buffer(x).Caption = to4
End If

If InStr(to4, "°coder5°coder5°") Then
to5 = Replace(to4, "°coder5°coder5°", "{+}")
buffer(x).Caption = to5
Else
to5 = to4
buffer(x).Caption = to5
End If
If InStr(to5, "°coder6°coder6°") Then
to6 = Replace(to5, "°coder6°coder6°", "{^}")
buffer(x).Caption = to6
Else
to6 = to5
buffer(x).Caption = to6
End If
If InStr(to6, "°coder7°coder7°") Then
to7 = Replace(to6, "°coder7°coder7°", "{%}")
buffer(x).Caption = to7
Else
to7 = to6
buffer(x).Caption = to7
End If
If InStr(to7, "°coder8°coder8°") Then
to8 = Replace(to7, "°coder8°coder8°", "{~}")
buffer(x).Caption = to8
End If
Next
End Sub
