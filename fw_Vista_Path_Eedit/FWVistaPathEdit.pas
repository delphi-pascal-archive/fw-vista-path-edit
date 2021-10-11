////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Unit Name : FWVistaPathEdit
//  * Purpose   : Класс эмулирующий поведение строки пути в висте
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2010.
//  * Version   : 1.03
//  * Home Page : http://rouse.drkb.ru
//  ****************************************************************************
//

unit FWVistaPathEdit;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  StdCtrls,
  Graphics,
  SysUtils,
  CommCtrl,
  ImgList,
  Forms;

type
  TFoldersInfo = record
    DirName: string;
    IconIndex: Integer;
  end;

  TFoldersData = class
  private
    FData: array of TFoldersInfo;
    function GetItem(Index: Integer): TFoldersInfo;
  public
    procedure Add(Value: TFoldersInfo);
    procedure Assign(Source: TFoldersData);
    function Count: Integer;
    function Text: string;
    property Item[Index: Integer]: TFoldersInfo read GetItem; default;
  end;

  TGSEditState = (esEdit, esAutoCompletion, esTab, esCombo);

  TOnNewPathEvent = procedure(Sender: TObject; const Path: string;
    var Accept: Boolean) of object;
  TOnQuerySubFolderEvent = procedure(Sender: TObject; const Path: string;
    Data: TFoldersData) of object;
  TOnQueryHistoryEvent = procedure(Sender: TObject;
    Data: TFoldersData) of object;
  TOnHasSubFoldersEvent = procedure(Sender: TObject; const Path: string;
    var HasFolders: Boolean) of object;
  TOnAutoCompleteEvent = procedure(Sender: TObject; const Path: string;
    AutoComplete: TStrings) of object;
  TMenuClickEvent = procedure(Sender: TObject; ItemIndex: Integer) of object;
  TOnPaintEvent = procedure(Sender: TObject; Canvas: TCanvas;
    ARect: TRect; Item: TFoldersInfo; Selected: Boolean) of object;
  TOnFillBackgroundEvent = procedure(Sender: TObject; Canvas: TCanvas;
    ARect: TRect) of object;

  TDoubleVertex = array [0..1] of TTriVertex;

  TButtonVertex = record
    UpGradient: TDoubleVertex;
    DownGradient: TDoubleVertex;
  end;

  TTabState = (tsInactive, tsHot, tsDropDownHot, tsPressed);

  TAbstractPainter = class(TComponent)
  public type
    TSupportMetod = (smBackground, smButtonGradient,
      smMenuPaint, smMenuFillBackground);
    TSupportMetods = set of TSupportMetod;
  strict private
    FImages: TCustomImageList;
    FChange: TNotifyEvent;
  protected
    function GetSupportedMedods: TSupportMetods; virtual; abstract;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DoChanged;
    property Images: TCustomImageList read FImages write FImages;
    property OnChange: TNotifyEvent read FChange write FChange;
  public
    procedure BackgroundPaint(Sender: TObject; Canvas: TCanvas;
      ARect: TRect; MouseHere: Boolean); virtual; abstract;
    procedure ButtonGradient(Sender: TObject; State: TTabState;
      var Button, DropDown: TButtonVertex;
      var CornerColor: TColor); virtual; abstract;
    procedure MenuPaint(Sender: TObject; Canvas: TCanvas;
      ARect: TRect; Item: TFoldersInfo; Selected: Boolean); virtual; abstract;
    procedure MenuFillBackground(Sender: TObject; Canvas: TCanvas;
      ARect: TRect); virtual; abstract;
    property SupportedMedods: TSupportMetods read GetSupportedMedods;
  end;

  TFWCustomVistaPathEdit = class(TCustomControl)
  private type
    TElements = (elNone, elLeftBtn, elLeftDropDown, elTab,
      elTabDropDown, elEditRect, elRightDropDown);
    TTransparentDirection = -1..1;
    TTab = record
      Caption: string;
      // ImageIndex: Integer;
      LeftOffset, Width: Integer;
      Hidden: Boolean;
      Transparent: Shortint;
      Direction: TTransparentDirection;
      State: TTabState;
      HasDropDown: Boolean;
    end;
    TTabs = array of TTab;
    TQueryElement = record
      TabIndex: Integer;
      Element: TElements;
    end;
    TBackgroundVertex = TDoubleVertex;
  strict private
    FPath: string;
    FEditState: TGSEditState;
    FEditHandle, FListHandle, FPopupHandle: TWinControl;
    FTabs: TTabs;
    FBackGround: TBitmap;
    FMouseHere, FMousePressed: Boolean;
    FCurrentElement: TQueryElement;
    FDropMarks: TImageList;
    FHasFolder: TOnHasSubFoldersEvent;
    FQueryFolder: TOnQuerySubFolderEvent;
    FNewPath: TOnNewPathEvent;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FImageIndex: Integer;
    FQueryHistory: TOnQueryHistoryEvent;
    FAuto: TOnAutoCompleteEvent;
    FHiddenCount: Integer;
    FPainter: TAbstractPainter;
    FLastPopupTabIndex: Integer;
    procedure SetPath(const Value: string);
    procedure SetEditState(const Value: TGSEditState);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: Integer);
    procedure ImageListChange(Sender: TObject);
    procedure SetMenuPainter(const Value: TAbstractPainter);
  protected
    // перекрытые методы
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMTime(var Message: TMessage); message WM_TIMER;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    // функциональные методы
    procedure CloseList;
    procedure CloseDropDown;
    procedure DropDownHistory;
    procedure DropDownAutoCompletion;
    procedure DropDownPopup;
    procedure ShowClosePopupEffect;
    procedure InitTabs;
    procedure InitDropDownMarks;
    function QueryElementAtPos(X: Integer; Y: Integer = -1): TQueryElement;
    procedure SetEditText(const Value: string);
    function GetEditHandle: TWinControl;
    function GetListHandle: TWinControl;
    procedure PerformMessageToCombo(var Message: TMessage);
    function IsPopupShowing: Boolean;
    function TabIndexToString(const TabIndex: Integer): string;
    function CurrentElement: TQueryElement;
  protected
    // врапперы событий
    procedure DoHasFolder(const TabIndex: Integer; var HasFolder: Boolean); overload; virtual;
    procedure DoHasFolder(const Path: string; var HasFolder: Boolean); overload; virtual;
    procedure DoQueryFolder(const TabIndex: Integer; Data: TFoldersData); overload; virtual;
    procedure DoQueryFolder(const Path: string; Data: TFoldersData); overload; virtual;
    procedure DoQueryHistory(Data: TFoldersData); virtual;
    procedure DoNewPath(const TabIndex: Integer; var Accept: Boolean); overload; virtual;
    procedure DoNewPath(const Path: string; var Accept: Boolean); overload; virtual;
    procedure DoAutoComplete(const Path: string; AutoComplete: TStrings); virtual;
    procedure DoMenuClick(Sender: TObject; ItemIndex: Integer); virtual;
    procedure DoPainterChange(Sender: TObject);
  protected
    // отрисовщики
    procedure DrawTabs;
    procedure DrawBackground;
    procedure DrawTabRect(Index: Integer; AColor: TColor = 0);
    procedure DrawComboState;
    function GetGrayVertex(R: TRect; Transparent: ShortInt;
      Down: Boolean): TBackgroundVertex;
    function GetBlueVertex(R: TRect; Transparent: ShortInt;
      Down: Boolean): TBackgroundVertex;
    function GetDarkVertex(R: TRect; Transparent: ShortInt;
      Down: Boolean): TBackgroundVertex;
    function GetCustomVertex(R: TRect; Transparent: ShortInt;
      Value: TDoubleVertex): TBackgroundVertex;
  protected
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property ExternalPainter: TAbstractPainter read FPainter
      write SetMenuPainter;
    property Text: string read FPath write SetPath;
    property OnAutoComplete: TOnAutoCompleteEvent read FAuto write FAuto;
    property OnHasFolders: TOnHasSubFoldersEvent read FHasFolder
      write FHasFolder;
    property OnNewPath: TOnNewPathEvent read FNewPath write FNewPath;
    property OnQueryFolders: TOnQuerySubFolderEvent
      read FQueryFolder write FQueryFolder;
    property OnQueryHistory: TOnQueryHistoryEvent
      read FQueryHistory write FQueryHistory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property EditState: TGSEditState read FEditState write SetEditState;
  end;

  TFWVistaPathEdit = class(TFWCustomVistaPathEdit)
  published
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ExternalPainter;
    property Images;
    property ImageIndex;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Text;
    property Visible;
    property OnAutoComplete;
    property OnHasFolders;
    property OnNewPath;
    property OnQueryFolders;
    property OnQueryHistory;
  end;

  function MakeFoldersInfo(DirName: string; IconIndex: Integer): TFoldersInfo;
  procedure Register;

implementation

uses
  RTLConsts,
  Math,
  Themes;

const
  LEFT_BTN_WIDTH = 35;
  RIGHT_BTN_WIDTH = 18;
  EMPTY_SECTOR_WIDTH = 50;
  BTN_NC_WIDTH = 6;
  BTN_NC_DOUBLE_WIDTH = BTN_NC_WIDTH shl 1;
  BTN_DROPDOWN_WIDTH = 16;
  BTN_MIN_WIDTH = 70;

  GradientRect: TGradientRect = (UpperLeft: 0; LowerRight: 1);

procedure Register;
begin
  RegisterComponents('Fangorn Wizards Lab', [TFWVistaPathEdit]);
end;

//
// =============================================================================
function MakeFoldersInfo(DirName: string; IconIndex: Integer): TFoldersInfo;
begin
  Result.DirName := DirName;
  Result.IconIndex := IconIndex;
end;

function Vertex(x, y: Longint; R, G, B, A: Byte): TTriVertex;
begin
  Result.x := x;
  Result.y := y;
  R := Byte(MulDiv(R, 5 - A, 5) + MulDiv(255, A, 5));
  G := Byte(MulDiv(G, 5 - A, 5) + MulDiv(255, A, 5));
  B := Byte(MulDiv(B, 5 - A, 5) + MulDiv(255, A, 5));
  Result.Red := R shl 8;
  Result.Green := G shl 8;
  Result.Blue := B shl 8;
  Result.Alpha := 0;
end;

type
  TSubEdit = class(TCustomEdit)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSubListBox = class(TCustomListBox)
  private
    FSearchText: String;
    FSearchTickCount: Longint;
    FBlockClick: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FIconIndexList: TList;
    FCurrentIconIndex: Integer;
    procedure SetImages(const Value: TCustomImageList);
  protected
    procedure ImageListChange(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Images: TCustomImageList read FImages write SetImages;
    property IconIndexList: TList read FIconIndexList;
    property CurrentIconIndex: Integer read FCurrentIconIndex;
  end;

  TSubPopupMenu = class(TCustomControl)
  private
    FData: TFoldersData;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FBackGround: TBitmap;
    FClick: TMenuClickEvent;
    FHeightList: TList;
    FSelectedIndex: Integer;
    FFillBackground: TOnFillBackgroundEvent;
    FPaint: TOnPaintEvent;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(const Value: TCustomImageList);
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  protected
    procedure DoPopoup(ItemIndex: Integer);
    procedure DrawRect(ARect: TRect; ItemIndex: Integer);
    procedure InvalidateRects;
    property Data: TFoldersData read FData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup(P: TPoint; Data: TFoldersData);
    procedure Hide;
    property Images: TCustomImageList read FImages write SetImages;
    property OnClick: TMenuClickEvent read FClick write FClick;
    property OnPaintItem: TOnPaintEvent read FPaint write FPaint;
    property OnFillBackground: TOnFillBackgroundEvent read FFillBackground write FFillBackground;
  end;

{ TSubEdit }

procedure TSubEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and
    (Message.Sender <> TFWCustomVistaPathEdit(Owner).GetListHandle) then
    TFWCustomVistaPathEdit(Owner).EditState := esTab;
end;

constructor TSubEdit.Create(AOwner: TComponent);
begin
  inherited;
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  DoubleBuffered := True;
end;

procedure TSubEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Accept: Boolean;
  M: TMessage;
begin
  if not Visible then Exit;
  case Key of
    VK_RETURN:
    begin
      Accept := True;
      TFWCustomVistaPathEdit(Owner).DoNewPath(Text, Accept);
      if Accept then
      begin
        TFWCustomVistaPathEdit(Owner).Text := Text;
        TFWCustomVistaPathEdit(Owner).EditState := esTab;
      end;
    end;
    VK_DOWN, VK_UP:
    if Focused then
    begin
      M.Msg := WM_KEYDOWN;
      M.WParam := Key;
      TFWCustomVistaPathEdit(Owner).PerformMessageToCombo(M);
      Key := 0;
    end;
    VK_ESCAPE:
      TFWCustomVistaPathEdit(Owner).EditState := esTab;
  end;
end;

procedure TSubEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_BACK, 32, $30..$39, $41..$5A, 220:
    begin
      if TFWCustomVistaPathEdit(Owner).EditState in [esEdit, esAutoCompletion] then
        TFWCustomVistaPathEdit(Owner).DropDownAutoCompletion;
    end;
  end;
end;

{ TSubListBox }

procedure TSubListBox.Click;
var
  I: Integer;
  Accept: Boolean;
begin
  inherited;
  if FBlockClick then
  begin
    FBlockClick := False;
    Exit;
  end;
  for I := 0 to Count - 1 do
    if Selected[I] then
    begin
      Accept := True;
      { TODO : Вот тут есть косяк.
        Если в обработчике DoNewPath поднять какой нибудь диалог,
        то он будет уровнем ниже чем наш TSubListBox, т.к. у последнего
        паретном выставлен десктоп. }
      TFWCustomVistaPathEdit(Owner).DoNewPath(Items[I], Accept);
      if Accept then
        TFWCustomVistaPathEdit(Owner).Text := Items[I];
      Break;
    end;
  TFWCustomVistaPathEdit(Owner).CloseDropDown;
end;

procedure TSubListBox.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and
    (Message.Sender <> TFWCustomVistaPathEdit(Owner).GetEditHandle) then
    TFWCustomVistaPathEdit(Owner).CloseDropDown;
end;

procedure TSubListBox.CNCommand(var Message: TWMCommand);
var
  I: Integer;
begin
  if TFWCustomVistaPathEdit(Owner).EditState = esEdit then
  begin
    inherited;
    Exit;
  end;
  if Message.NotifyCode = LBN_SELCHANGE then
  begin
    for I := 0 to Count - 1 do
      if Selected[I] then
      begin
        FCurrentIconIndex := I;
        Break;
      end;
    if TFWCustomVistaPathEdit(Owner).EditState = esAutoCompletion then
    begin
      TFWCustomVistaPathEdit(Owner).SetEditText(ExtractFilePath(
        TSubEdit(TFWCustomVistaPathEdit(Owner).GetEditHandle).Text) + Items[I]);
    end
    else
    begin
      TFWCustomVistaPathEdit(Owner).SetEditText(Items[I]);
      TFWCustomVistaPathEdit(Owner).DrawTabs;
      TFWCustomVistaPathEdit(Owner).Invalidate;
    end;
  end;
  inherited;
end;

procedure TSubListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Exclude(State, odFocused);
    TOwnerDrawState(LongRec(itemState).Lo) := State;
  end;
  inherited;
end;

constructor TSubListBox.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentIconIndex := -1;
  ControlStyle := ControlStyle + [csNeedsBorderPaint] - [csSetCaption];
  Style := lbOwnerDrawFixed;
  DoubleBuffered := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FIconIndexList := TList.Create; 
end;

procedure TSubListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS or CS_DROPSHADOW;
  end;
end;

procedure TSubListBox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

destructor TSubListBox.Destroy;
begin
  FIconIndexList.Free;
  FImageChangeLink.Free;
  inherited;
end;

procedure TSubListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if odSelected in State then
    Canvas.Brush.Color := RGB(51, 153, 255)
  else
    Canvas.Brush.Color := clWhite;
  Canvas.FillRect(Rect);
  Canvas.TextOut(18, Rect.Top + 1, Items[Index]);
  if FImages <> nil then
    if FIconIndexList.Count > Index then
  FImages.Draw(Canvas, 1, Rect.Top, Integer(FIconIndexList[Index]));
end;

procedure TSubListBox.ImageListChange(Sender: TObject);
begin
  Repaint;
end;

procedure TSubListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_ESCAPE:
      TFWCustomVistaPathEdit(Owner).CloseDropDown;
    VK_RETURN:
      Click;
  else
    FBlockClick := True;
  end;
end;

procedure TSubListBox.KeyPress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
    begin
      TickCount := GetTickCount;
      if TickCount - FSearchTickCount > 2000 then
        FSearchText := '';
      FSearchTickCount := TickCount;
      if Length(FSearchText) < 32 then
        FSearchText := FSearchText + Key;
      SendMessage(Handle, LB_SelectString,
        WORD(-1), Longint(PChar(FSearchText)));
      Key := #0;
    end;
  end;
  inherited Keypress(Key);
end;

procedure TSubListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ItemIndex: Integer;
begin
  inherited;
  ItemIndex := ItemAtPos(Point(X, Y), True);
  if ItemIndex >= 0 then
    if not Selected[ItemIndex] then
      Selected[ItemIndex] := True;
end;

procedure TSubListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil;
end;

procedure TSubListBox.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    Invalidate;
  end;
  FCurrentIconIndex := -1;
end;

procedure TSubListBox.WMActivateApp(var Message: TWMActivateApp);
begin
  if not Message.Active then
    TFWCustomVistaPathEdit(Owner).CloseDropDown;
end;

{ TGSCustomPopupMenu }

procedure TSubPopupMenu.CMCancelMode(var Message: TCMCancelMode);
begin
  if Message.Sender <> Self then
  begin
    TFWCustomVistaPathEdit(Owner).ShowClosePopupEffect;
    Hide;
  end;
end;

procedure TSubPopupMenu.WMActivateApp(var Message: TWMActivateApp);
begin
  inherited;
  if not Message.Active then
  begin
    TFWCustomVistaPathEdit(Owner).ShowClosePopupEffect;
    Hide;
  end;
end;

procedure TSubPopupMenu.CMMouseLeave(var Message: TMessage);
begin
  FSelectedIndex := -1;
  InvalidateRects;
end;

constructor TSubPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := True;
  DoubleBuffered := True;
  Visible := False;
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque,
    csDoubleClicks, csNeedsBorderPaint];
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FBackGround := TBitmap.Create;
  FBackGround.PixelFormat := pf32bit;
  FData := TFoldersData.Create;
  FHeightList := TList.Create;
end;

procedure TSubPopupMenu.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS or CS_DROPSHADOW;
  end;
end;

procedure TSubPopupMenu.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

destructor TSubPopupMenu.Destroy;
begin
  FHeightList.Free;
  FData.Free;
  FBackGround.Free;
  FImageChangeLink.Free;
  inherited;
end;

procedure TSubPopupMenu.DoPopoup(ItemIndex: Integer);
begin
  if Assigned(FClick) then
    FClick(Self, ItemIndex);
end;

procedure TSubPopupMenu.DrawRect(ARect: TRect; ItemIndex: Integer);
var
  BorderRect: TRect;
  Separator: Boolean;
begin
  if Assigned(FPaint) then
  begin
    FPaint(Self, FBackGround.Canvas, ARect, FData[ItemIndex],
      FSelectedIndex = ItemIndex);
    Exit;
  end;
  Separator := FData[ItemIndex].DirName = '-';
  if (FSelectedIndex = ItemIndex) and not Separator then
  begin
    FBackGround.Canvas.Brush.Color := RGB(51, 153, 255);
    FBackGround.Canvas.FillRect(ARect);
  end
  else
  begin
    FBackGround.Canvas.Brush.Color := clWhite;
    FBackGround.Canvas.FillRect(ARect);
    BorderRect := ARect;
    BorderRect.Right := BorderRect.Left + 25;
    FBackGround.Canvas.Brush.Color := RGB(239, 239, 239);
    FBackGround.Canvas.FillRect(BorderRect);
    FBackGround.Canvas.Pen.Color := RGB(198, 199, 198);
    FBackGround.Canvas.MoveTo(BorderRect.Right, BorderRect.Top);
    FBackGround.Canvas.LineTo(BorderRect.Right, BorderRect.Bottom);
    FBackGround.Canvas.Brush.Color := clWhite;
  end;
  if Separator then
  begin
    FBackGround.Canvas.Pen.Color := RGB(198, 199, 198);
    FBackGround.Canvas.MoveTo(BorderRect.Right + 4, BorderRect.Top + 2);
    FBackGround.Canvas.LineTo(ARect.Right, BorderRect.Top + 2);
  end
  else
  begin
    if FImages <> nil then
      FImages.Draw(FBackGround.Canvas, 7, ARect.Top + 5,
        FData[ItemIndex].IconIndex);
    FBackGround.Canvas.TextOut(36, ARect.Top + 5, FData[ItemIndex].DirName);
  end;
end;

procedure TSubPopupMenu.Hide;
begin
  Visible := False;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
end;

procedure TSubPopupMenu.ImageListChange(Sender: TObject);
begin
  InvalidateRects;
end;

procedure TSubPopupMenu.InvalidateRects;
var
  I, Y: Integer;
  ARect, BorderRect: TRect;
begin
  ARect := GetClientRect;
  if not Assigned(FFillBackground) then
  begin
    FBackGround.Canvas.Brush.Color := clWhite;
    FBackGround.Canvas.FillRect(ARect);
    BorderRect := ARect;
    BorderRect.Right := BorderRect.Left + 27;
    FBackGround.Canvas.Brush.Color := RGB(239, 239, 239);
    FBackGround.Canvas.FillRect(BorderRect);
    FBackGround.Canvas.Pen.Color := RGB(198, 199, 198);
    FBackGround.Canvas.MoveTo(BorderRect.Right, BorderRect.Top);
    FBackGround.Canvas.LineTo(BorderRect.Right, BorderRect.Bottom);
    FBackGround.Canvas.Brush.Color := clWhite;
  end
  else
    FFillBackground(Self, FBackGround.Canvas, ARect);
  Y := 2;
  for I := 0 to FData.Count - 1 do
  begin
    DrawRect(Rect(2, Y, ClientWidth - 2, Y + Integer(FHeightList[I])), I);
    Inc(Y, Integer(FHeightList[I]));
  end;
  Invalidate;
end;

procedure TSubPopupMenu.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP:
    begin
      Dec(FSelectedIndex);
      if FSelectedIndex < 0 then
        FSelectedIndex := FData.Count - 1;
      InvalidateRects;
    end;
    VK_DOWN:
    begin
      Inc(FSelectedIndex);
      if FSelectedIndex > FData.Count - 1 then
        FSelectedIndex := 0;
      InvalidateRects;
    end;
    VK_RETURN:
    begin
      DoPopoup(FSelectedIndex);
      TFWCustomVistaPathEdit(Owner).ShowClosePopupEffect;
      Hide;
    end;
  end;
end;

procedure TSubPopupMenu.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  DoPopoup(FSelectedIndex);
  TFWCustomVistaPathEdit(Owner).ShowClosePopupEffect;
  Hide;
end;

procedure TSubPopupMenu.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  X := 2;
  for I := 0 to FHeightList.Count - 1 do
  begin
    Inc(X, Integer(FHeightList[I]));
    if Y < X then
    begin
      if FSelectedIndex <> I then
      begin
        FSelectedIndex := I;
        InvalidateRects;
      end;
      Break;
    end;
  end;
end;

procedure TSubPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil;
end;

procedure TSubPopupMenu.Paint;
begin
  inherited;
  BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
    FBackGround.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TSubPopupMenu.Popup(P: TPoint; Data: TFoldersData);
var
  I, MaxWidth, MaxHeight: Integer;
begin
  FSelectedIndex := -1;
  FData.Assign(Data);
  FHeightList.Clear;
  MaxWidth := 0;
  MaxHeight := 6;
  for I := 0 to FData.Count - 1 do
  begin
    MaxWidth := Max(MaxWidth, Canvas.TextWidth(FData[I].DirName));
    if FData[I].DirName <> '-' then
      FHeightList.Add(Pointer(24))
    else
      FHeightList.Add(Pointer(5));
    Inc(MaxHeight, Integer(FHeightList[I]));
  end;
  Inc(MaxWidth, 70);
  Height := MaxHeight;
  Width := MaxWidth;
  FBackGround.Width := MaxWidth;
  FBackGround.Height := MaxHeight;
  Visible := True;
  SetWindowPos(Handle, HWND_TOP, P.X, P.Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  SetFocus;
  InvalidateRects;
end;

procedure TSubPopupMenu.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  InvalidateRects;
end;

procedure TSubPopupMenu.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS;
end;

procedure TSubPopupMenu.WMNCPaint(var Message: TMessage);
var
  ADrawRect: TRect;
  DC: HDC;
  Details: TThemedElementDetails;
begin
  if not ThemeServices.ThemesEnabled then
  begin
    inherited;
    Exit;
  end;
  with Self do
  begin
    GetWindowRect(Handle, ADrawRect);
    OffsetRect(ADrawRect, -ADrawRect.Left, -ADrawRect.Top);
    DC := GetWindowDC(Handle);
    try
      with ADrawRect do
        ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
      Details := ThemeServices.GetElementDetails(teEditTextNormal);
      ThemeServices.DrawElement(DC, Details, ADrawRect);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

{ TFoldersData }

procedure TFoldersData.Add(Value: TFoldersInfo);
var
  ACount: Integer;
begin
  ACount := Count;
  SetLength(FData, ACount + 1);
  FData[ACount] := Value;
end;

procedure TFoldersData.Assign(Source: TFoldersData);
var
  ACount, I: Integer;
begin
  ACount := Source.Count;
  SetLength(FData, ACount);
  for I := 0 to ACount - 1 do
    FData[I] := Source[I];
end;

function TFoldersData.Count: Integer;
begin
  Result := Length(FData);
end;

function TFoldersData.GetItem(Index: Integer): TFoldersInfo;
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  Result := FData[Index];
end;

function TFoldersData.Text: string;
var
  I, ACount: Integer;
begin
  Result := '';
  ACount := Count;
  if ACount = 0 then Exit;  
  for I := 0 to ACount - 2 do
    Result := Result + Item[I].DirName + sLineBreak;
  Result := Result + Item[ACount - 1].DirName;
end;

{ TGSVistaPathEdit }

procedure TFWCustomVistaPathEdit.CloseList;
begin
  if csDesigning in ComponentState then Exit;
  if FListHandle.Visible then
  begin
    FListHandle.Visible := False;
    SetWindowPos(FListHandle.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  end;
  if FPopupHandle.Visible then
  begin
    FPopupHandle.Visible := False;
    SetWindowPos(FPopupHandle.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  end;
end;

procedure TFWCustomVistaPathEdit.CloseDropDown;
begin
  CloseList;
  if EditState <> esTab then  
    EditState := esTab;
end;

procedure TFWCustomVistaPathEdit.ShowClosePopupEffect;
begin
  if not IsPopupShowing then Exit;  
  if Length(FTabs) <= FLastPopupTabIndex then Exit;
  if FLastPopupTabIndex < 0 then Exit;
  FTabs[FLastPopupTabIndex].State := tsHot;
  FTabs[FLastPopupTabIndex].Direction := 1;
  FTabs[FLastPopupTabIndex].Transparent := 0;
  SetTimer(Handle, 1, 50, nil);
end;

function TFWCustomVistaPathEdit.TabIndexToString(
  const TabIndex: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to TabIndex do
    Result := Result + FTabs[I].Caption + '\';
end;

procedure TFWCustomVistaPathEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if Message.Sender <> FListHandle then
  begin
    CloseDropDown;
    ShowClosePopupEffect;
  end;
end;

procedure TFWCustomVistaPathEdit.CMExit(var Message: TCMExit);
begin
  if csDesigning in ComponentState then Exit;
  FMouseHere := False;
  EditState := esTab;
  ShowClosePopupEffect;
  CloseDropDown;
end;

procedure TFWCustomVistaPathEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  FMouseHere := True;
  DrawTabs;
  Invalidate;
end;

procedure TFWCustomVistaPathEdit.CMMouseLeave(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if IsPopupShowing then Exit;
  for I := 0 to Length(FTabs) - 1 do
  begin
    FTabs[I].Transparent := 0;
    FTabs[I].Direction := 0;
    FTabs[I].State := tsInactive;
  end;  
  FMouseHere := False;
  FCurrentElement.TabIndex := -1;
  DrawTabs;
  Invalidate;
end;

constructor TFWCustomVistaPathEdit.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := True;
  Width := 175;
  Height := 22;
  Constraints.MinWidth := 175;
  DoubleBuffered := True;
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque,
    csDoubleClicks, csNeedsBorderPaint];
  if not (csDesigning in ComponentState) then
  begin
    FEditHandle := TSubEdit.Create(Self);
    FEditHandle.Visible := False;
    FEditHandle.Parent := Self;
    FListHandle := TSubListBox.Create(Self);
    FListHandle.Parent := Self;
    FListHandle.Visible := False;
    FPopupHandle := TSubPopupMenu.Create(Self);
    FPopupHandle.Parent := Self;
    TSubPopupMenu(FPopupHandle).OnClick := DoMenuClick;
  end;
  FBackGround := TBitmap.Create;
  FBackGround.PixelFormat := pf32bit;
  FCurrentElement.TabIndex := -1;
  InitDropDownMarks;
  FEditState := esTab;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageIndex := -1;
end;

procedure TFWCustomVistaPathEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := ((Style or WS_CLIPCHILDREN or WS_TABSTOP) or
      WS_BORDER) or LBS_OWNERDRAWFIXED or WS_CLIPCHILDREN;
    if NewStyleControls and Ctl3D then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

function TFWCustomVistaPathEdit.CurrentElement: TQueryElement;
begin
  Result := FCurrentElement;
end;

destructor TFWCustomVistaPathEdit.Destroy;
begin
  FImageChangeLink.Free;
  FDropMarks.Free;
  FBackGround.Free;
  if not (csDesigning in ComponentState) then
  begin
    FPopupHandle.Free;
    FEditHandle.Free;
    FListHandle.Free;
  end;
  inherited;
end;

procedure TFWCustomVistaPathEdit.DoAutoComplete(const Path: string;
  AutoComplete: TStrings);
begin
  if Assigned(FAuto) then
    FAuto(Self, Path, AutoComplete);
end;

procedure TFWCustomVistaPathEdit.DoHasFolder(const TabIndex: Integer;
  var HasFolder: Boolean);
begin
  DoHasFolder(TabIndexToString(TabIndex), HasFolder);
end;

procedure TFWCustomVistaPathEdit.DoHasFolder(const Path: string;
  var HasFolder: Boolean);
begin
  if Assigned(FHasFolder) then
    FHasFolder(Self, Path, HasFolder);
end;

procedure TFWCustomVistaPathEdit.DoMenuClick(Sender: TObject; ItemIndex: Integer);
var
  Accept: Boolean;
  Path: string;
  I: Integer;
begin
  Path := '';
  if (FLastPopupTabIndex = 0) and (FHiddenCount > ItemIndex) then
  begin
    for I := 1 to FHiddenCount - ItemIndex do
      Path := Path + FTabs[I].Caption + '\';
  end
  else
  begin
    for I := 1 to FLastPopupTabIndex do
      Path := Path + FTabs[I].Caption + '\';
    Path := Path + TSubPopupMenu(FPopupHandle).Data[ItemIndex].DirName + '\';
  end;
  Accept := True;
  DoNewPath(Path, Accept);
  if Accept then
    Text := Path;
  ShowClosePopupEffect;
end;

procedure TFWCustomVistaPathEdit.DoNewPath(const TabIndex: Integer;
  var Accept: Boolean);
begin
  DoNewPath(TabIndexToString(TabIndex), Accept);
end;

procedure TFWCustomVistaPathEdit.DoNewPath(const Path: string;
  var Accept: Boolean);
begin
  if Assigned(FNewPath) then
    FNewPath(Self, Path, Accept);
end;

procedure TFWCustomVistaPathEdit.DoPainterChange(Sender: TObject);
begin
  DrawTabs;
  Invalidate;
end;

procedure TFWCustomVistaPathEdit.DoQueryFolder(const Path: string;
  Data: TFoldersData);
begin
  if Assigned(FQueryFolder) then
    FQueryFolder(Self, Path, Data);
end;

procedure TFWCustomVistaPathEdit.DoQueryFolder(const TabIndex: Integer;
  Data: TFoldersData);
begin
  DoQueryFolder(TabIndexToString(TabIndex), Data);
end;

procedure TFWCustomVistaPathEdit.DoQueryHistory(Data: TFoldersData);
begin
  if Assigned(FQueryHistory) then
    FQueryHistory(Self, Data);
end;

procedure TFWCustomVistaPathEdit.DrawBackground;
var
  BackgroundVertex: TBackgroundVertex;
begin
  // Заливка бэкграунда
  if EditState = esTab then
  begin
    if ExternalPainter <> nil then
      if smBackground in ExternalPainter.SupportedMedods then
      begin
        ExternalPainter.BackgroundPaint(Self,
          FBackGround.Canvas, ClientRect, FMouseHere);
        Exit;
      end;   

    // Если курсор находится над компонентом, то делаем сплошную белую заливку
    if FMouseHere then
    begin
      FBackGround.Canvas.Brush.Color := clWhite;
      FBackGround.Canvas.FillRect(ClientRect);
    end
    else
    begin
      // в противном случае градиент
      BackgroundVertex[0] := Vertex(0, 0, 250, 253, 254, 0);
      BackgroundVertex[1] := Vertex(ClientWidth, ClientHeight,
        232, 245, 252, 0);
      GradientFill(FBackGround.Canvas.Handle, @BackgroundVertex[0], 2,
        @GradientRect, 1, GRADIENT_FILL_RECT_V);
    end;
  end
  else
  begin
    // если мы в режиме редактирования - делаем сплошную белую заливку
    FBackGround.Canvas.Brush.Color := clWhite;
    FBackGround.Canvas.FillRect(ClientRect);
  end;
end;

procedure TFWCustomVistaPathEdit.DrawComboState;
var
  I: Integer;
  BackgroundVertex: TBackgroundVertex;
  R: TRect;
  ButtonVertex, DropDownVertex: TButtonVertex;
  UseExternalPaiter: Boolean;
  CornerColor: TColor;
begin
  I := TSubListBox(FListHandle).CurrentIconIndex;

  if Assigned(FImages) then
  begin
    if I < 0 then
      FImages.Draw(FBackGround.Canvas, 2, 1, FImageIndex)
    else
    begin
      I := Integer(TSubListBox(FListHandle).IconIndexList[I]);
      FImages.Draw(FBackGround.Canvas, 2, 1, I);
    end;
  end;      

  I := Length(FTabs) - 1;
  R.Top := 0;
  R.Left := FTabs[I].LeftOffset;
  R.Right := R.Left + RIGHT_BTN_WIDTH;
  R.Bottom := ClientHeight shr 1;

  UseExternalPaiter := False;
  if ExternalPainter <> nil then
    UseExternalPaiter := smButtonGradient in ExternalPainter.SupportedMedods;

  CornerColor := 0;
  if UseExternalPaiter then
    ExternalPainter.ButtonGradient(Self, tsPressed,
      ButtonVertex, DropDownVertex, CornerColor);

  if UseExternalPaiter then
    BackgroundVertex := GetCustomVertex(R, 0, ButtonVertex.UpGradient)
  else
    BackgroundVertex := GetDarkVertex(R, 0, False);
  GradientFill(FBackGround.Canvas.Handle, @BackgroundVertex[0], 2,
    @GradientRect, 1, GRADIENT_FILL_RECT_V);
  R.Top := R.Bottom;
  R.Bottom := ClientHeight;
  if UseExternalPaiter then
    BackgroundVertex := GetCustomVertex(R, 0, ButtonVertex.DownGradient)
  else
    BackgroundVertex := GetDarkVertex(R, 0, True);
  GradientFill(FBackGround.Canvas.Handle, @BackgroundVertex[0], 2,
    @GradientRect, 1, GRADIENT_FILL_RECT_V);
  FDropMarks.Draw(FBackGround.Canvas, R.Left +
    BTN_NC_WIDTH, BTN_NC_WIDTH + 1, 1);

  DrawTabRect(I, CornerColor);
end;

procedure TFWCustomVistaPathEdit.DrawTabRect(Index: Integer;
  AColor: TColor = 0);
var
  BackgroundVertex: array [0..1] of TTriVertex;
  PenPosX: Integer;
begin
  // Отрисовка линий границы активного таба,
  if AColor = 0 then
    AColor := RGB(60, 127, 177);
  if FTabs[Index].Direction = 0 then
    FBackGround.Canvas.Pen.Color := AColor
  else
  begin
    BackgroundVertex[0] := Vertex(0, 0,
      GetRValue(AColor), GetGValue(AColor), GetBValue(AColor),
      FTabs[Index].Transparent);
    FBackGround.Canvas.Pen.Color := RGB(
      BackgroundVertex[0].Red shr 8,
      BackgroundVertex[0].Green shr 8,
      BackgroundVertex[0].Blue shr 8);
  end;
  PenPosX := FTabs[Index].LeftOffset;

  // У самого первого таба границу с лева не рисуем
  if Index > 0 then
  begin
    FBackGround.Canvas.MoveTo(PenPosX, 0);
    FBackGround.Canvas.LineTo(PenPosX, ClientHeight);
  end;
  Inc(PenPosX, FTabs[Index].Width);
  FBackGround.Canvas.MoveTo(PenPosX, 0);
  FBackGround.Canvas.LineTo(PenPosX, ClientHeight);
  if FTabs[Index].HasDropDown then
  begin
    Dec(PenPosX, BTN_DROPDOWN_WIDTH);
    FBackGround.Canvas.MoveTo(PenPosX, 0);
    FBackGround.Canvas.LineTo(PenPosX, ClientHeight);
  end;
end;

procedure TFWCustomVistaPathEdit.DrawTabs;
var
  BackgroundVertex: TBackgroundVertex;
  I, X1, Y1, X2, Y2: Integer;
  R: TRect;
  ButtonVertex, DropDownVertex: TButtonVertex;
  UseExternalPaiter: Boolean;
  CornerColor: TColor;
begin
  DrawBackground;

  UseExternalPaiter := False;
  if ExternalPainter <> nil then
    UseExternalPaiter := smButtonGradient in ExternalPainter.SupportedMedods;

  // В режиме раскрытого комбобокса отрисовываем только иконку
  // выбранного элемента в комбобоксе и зажатую кнопку
  if EditState = esCombo then
  begin
    DrawComboState;
    Exit;
  end;

  for I := 0 to Length(FTabs) - 1 do
  begin
    if (I = 0) and (EditState <> esTab) then Continue;
    if FTabs[I].Hidden then Continue;
    if (FTabs[I].State <> tsInactive) and FMouseHere then
    begin

      CornerColor := 0;
      if UseExternalPaiter then
        ExternalPainter.ButtonGradient(Self, FTabs[I].State,
          ButtonVertex, DropDownVertex, CornerColor);

      DrawTabRect(I, CornerColor);

      // Заливка фона у активных табов
      if FTabs[I].State <> tsInactive then
      begin
        // Верхняя половина градиента кнопки
        X1 := FTabs[I].LeftOffset + 2;
        if FTabs[I].HasDropDown then
          X2 := X1 - 3 + FTabs[I].Width - BTN_DROPDOWN_WIDTH
        else
          X2 := X1 - 3 + FTabs[I].Width;
        Y1 := 0;
        Y2 := ClientHeight shr 1;
        if I > 0 then
        begin
          // Отрисовываем стиль tsDropDownHot во всех случаях,
          // за исключением затухания таба
          if UseExternalPaiter then
            BackgroundVertex := GetCustomVertex(Rect(X1, Y1, X2, Y2),
              FTabs[I].Transparent, ButtonVertex.UpGradient)
          else
          begin
            if (FTabs[I].State = tsDropDownHot) and
              (FTabs[I].Transparent <= 0) then
              BackgroundVertex := GetGrayVertex(Rect(X1, Y1, X2, Y2),
                FTabs[I].Transparent, False)
            else
              if FTabs[I].State = tsPressed then
                BackgroundVertex := GetDarkVertex(Rect(X1, Y1, X2, Y2),
                  0, False)
              else
                BackgroundVertex := GetBlueVertex(Rect(X1, Y1, X2, Y2),
                  FTabs[I].Transparent, False);
          end;
          GradientFill(FBackGround.Canvas.Handle, @BackgroundVertex[0], 2,
            @GradientRect, 1, GRADIENT_FILL_RECT_V);
        end;
        // Нижняя граница градиента кнопки
        Y1 := Y2;
        Y2 := ClientHeight;
        if I > 0 then
        begin
          // Отрисовываем стиль tsDropDownHot во всех случаях,
          // за исключением затухания таба
          if UseExternalPaiter then
            BackgroundVertex := GetCustomVertex(Rect(X1, Y1, X2, Y2),
              FTabs[I].Transparent, ButtonVertex.DownGradient)
          else
          begin
            if (FTabs[I].State = tsDropDownHot) and
              (FTabs[I].Transparent <= 0) then
              BackgroundVertex := GetGrayVertex(Rect(X1, Y1, X2, Y2),
                FTabs[I].Transparent, True)
            else
              if FTabs[I].State = tsPressed then
                BackgroundVertex := GetDarkVertex(Rect(X1, Y1, X2, Y2),
                  0, True)
              else
                BackgroundVertex := GetBlueVertex(Rect(X1, Y1, X2, Y2),
                  FTabs[I].Transparent, True);
          end;
          GradientFill(FBackGround.Canvas.Handle, @BackgroundVertex[0], 2,
            @GradientRect, 1, GRADIENT_FILL_RECT_V);
        end;
        // Нижняя половина градиента дропдауна
        if FTabs[I].HasDropDown then
        begin
          X1 := X2 + 3;
          X2 := X1 + BTN_DROPDOWN_WIDTH - 3;
          if UseExternalPaiter then
            BackgroundVertex := GetCustomVertex(Rect(X1, Y1, X2, Y2),
              FTabs[I].Transparent, DropDownVertex.DownGradient)
          else
          begin
            if FTabs[I].State = tsPressed then
              BackgroundVertex := GetDarkVertex(Rect(X1, Y1, X2, Y2),
                0, True)
            else
              BackgroundVertex := GetBlueVertex(Rect(X1, Y1, X2, Y2),
                FTabs[I].Transparent, True);
          end;
          GradientFill(FBackGround.Canvas.Handle, @BackgroundVertex[0], 2,
            @GradientRect, 1, GRADIENT_FILL_RECT_V);
          // Верхняя половина градиента дропдауна
          Y1 := 0;
          Y2 := ClientHeight shr 1;
          if UseExternalPaiter then
            BackgroundVertex := GetCustomVertex(Rect(X1, Y1, X2, Y2),
              FTabs[I].Transparent, DropDownVertex.UpGradient)
          else
          begin
            if FTabs[I].State = tsPressed then
              BackgroundVertex := GetDarkVertex(Rect(X1, Y1, X2, Y2),
                0, False)
            else
              BackgroundVertex := GetBlueVertex(Rect(X1, Y1, X2, Y2),
                FTabs[I].Transparent, False);
          end;
          GradientFill(FBackGround.Canvas.Handle, @BackgroundVertex[0], 2,
            @GradientRect, 1, GRADIENT_FILL_RECT_V);
        end;
      end;
    end;

    // Отрисовка различных вариантов маркера

    // Если присутствуют скрытые табы, то первый маркер рисуется вот так:
    if FTabs[1].Hidden and (I = 0) then
    begin
      FDropMarks.Draw(FBackGround.Canvas, FTabs[I].LeftOffset +
        FTabs[I].Width - BTN_DROPDOWN_WIDTH + 4, BTN_NC_WIDTH + 1, 2);
      Continue;
    end;

    // Если таб обычный и имеется маркер, то маркеры рисуются двух видов
    if FTabs[I].HasDropDown then
    begin
      // если таб нажат
      if FTabs[I].State <> tsPressed then
        FDropMarks.Draw(FBackGround.Canvas, FTabs[I].LeftOffset +
          FTabs[I].Width - BTN_DROPDOWN_WIDTH + BTN_NC_WIDTH + 1,
          BTN_NC_WIDTH, 0)
      else
        // и обычный вариант
        FDropMarks.Draw(FBackGround.Canvas, FTabs[I].LeftOffset +
          FTabs[I].Width - BTN_DROPDOWN_WIDTH + BTN_NC_WIDTH - 1,
          BTN_NC_WIDTH + 1, 1);
    end
    else
      // для последнего таба отрисовывается всегда выпалающий маркер
      if I = Length(FTabs) - 1 then
        FDropMarks.Draw(FBackGround.Canvas, FTabs[I].LeftOffset +
          BTN_NC_WIDTH, BTN_NC_WIDTH + 1, 1);

    // Текст таба
    FBackGround.Canvas.Brush.Style := bsClear;
    try
      R := Rect(FTabs[I].LeftOffset, 0,
        FTabs[I].LeftOffset + FTabs[I].Width, ClientHeight);
      InflateRect(R, -BTN_NC_WIDTH, 0);
      OffsetRect(R, 0, 2);
      if FTabs[I].HasDropDown then      
        Dec(R.Right, BTN_DROPDOWN_WIDTH);
      DrawText(FBackGround.Canvas.Handle, PChar(FTabs[I].Caption),
        Length(FTabs[I].Caption), R, DT_END_ELLIPSIS or DT_WORD_ELLIPSIS);
    finally
      FBackGround.Canvas.Brush.Style := bsSolid;
    end;
  end;

  // орисовка картинки для рутового таба
    if FImages <> nil then
      if FTabs[0].State = tsPressed then
        FImages.Draw(FBackGround.Canvas, 3, 2, FImageIndex)
      else
        FImages.Draw(FBackGround.Canvas, 2, 1, FImageIndex);
end;

procedure TFWCustomVistaPathEdit.DropDownAutoCompletion;
var
  AutoComplete: TStringList;
  P: TPoint;
  Y, I, MaxWidth: Integer;
begin
  if csDesigning in ComponentState then Exit;
  AutoComplete := TStringList.Create;
  try
    TSubListBox(FListHandle).IconIndexList.Clear;
    DoAutoComplete(TSubEdit(FEditHandle).Text, AutoComplete);
    if AutoComplete.Count = 0 then
    begin
      CloseList;
      Exit;
    end;
    MaxWidth := 0;
    for I := 0 to AutoComplete.Count - 1 do
      MaxWidth := Max(MaxWidth, Canvas.TextWidth(AutoComplete[I]));
    Inc(MaxWidth, 40);
    EditState := esAutoCompletion;
    TSubListBox(FListHandle).Images := Images;
    FListHandle.Width := MaxWidth;
    FListHandle.Height :=
      AutoComplete.Count * TSubListBox(FListHandle).ItemHeight + 2;
    P := ClientToScreen(Point(0, 0));
    Y := P.Y + Height - 1;
    if Y + FListHandle.Height > Screen.Height then
      FListHandle.Height := Screen.Height - Y - 40;
    TSubListBox(FListHandle).Items.Text := AutoComplete.Text;
    CloseList;
    SetWindowPos(FListHandle.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListHandle.Visible := True;
  finally
    AutoComplete.Free;
  end;
end;

procedure TFWCustomVistaPathEdit.DropDownHistory;
var
  Data: TFoldersData;
  P: TPoint;
  Y, I: Integer;
begin
  if csDesigning in ComponentState then Exit;
  Data := TFoldersData.Create;
  try
    TSubListBox(FListHandle).IconIndexList.Clear;
    DoQueryHistory(Data);
    if Data.Count = 0 then Exit;
    for I := 0 to Data.Count - 1 do
      TSubListBox(FListHandle).IconIndexList.Add(Pointer(Data[I].IconIndex));
    EditState := esCombo;
    TSubListBox(FListHandle).Images := Images;
    FListHandle.Width := ClientWidth;
    FListHandle.Height :=
      Data.Count * TSubListBox(FListHandle).ItemHeight + 2;
    P := ClientToScreen(Point(0, 0));
    Y := P.Y + Height - 1;
    if Y + FListHandle.Height > Screen.Height then
      Y := P.Y - FListHandle.Height;
    SetWindowPos(FListHandle.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE);
    TSubListBox(FListHandle).Items.Text := Data.Text;
    TSubListBox(FListHandle).Selected[0] := True;
    AnimateWindow(FListHandle.Handle, 200, AW_SLIDE or AW_VER_POSITIVE);
    FListHandle.Visible := True;
    SetFocus;
  finally
    Data.Free;
  end;
end;

procedure TFWCustomVistaPathEdit.DropDownPopup();
var
  Data: TFoldersData;
  P: TPoint;
  I: Integer;
begin
  if csDesigning in ComponentState then Exit;
  if FCurrentElement.TabIndex < 0 then Exit;
  if FCurrentElement.TabIndex >= Length(FTabs) then Exit;
  Data := TFoldersData.Create;
  try

    if FCurrentElement.TabIndex = 0 then
      if FHiddenCount > 0 then
      begin
        for I := FHiddenCount downto 1 do
          Data.Add(MakeFoldersInfo(FTabs[I].Caption, -1));
        Data.Add(MakeFoldersInfo('-', -1));
      end;

    DoQueryFolder(FCurrentElement.TabIndex, Data);
    if Data.Count = 0 then Exit;
    FLastPopupTabIndex := FCurrentElement.TabIndex;
    TSubPopupMenu(FPopupHandle).Images := Images;
    P := ClientToScreen(Point(
      FTabs[FCurrentElement.TabIndex].LeftOffset, Height - 2));
    TSubPopupMenu(FPopupHandle).Popup(P, Data);
    FTabs[FCurrentElement.TabIndex].State := tsPressed;
    FTabs[FCurrentElement.TabIndex].Direction := 0;
    FTabs[FCurrentElement.TabIndex].Transparent := 0;
    DrawTabs;
    Invalidate;
  finally
    Data.Free;
  end;
end;

function TFWCustomVistaPathEdit.GetBlueVertex(R: TRect; Transparent: ShortInt;
  Down: Boolean): TBackgroundVertex;
begin
  if Transparent < 0 then
    Transparent := 0;
  if Down then
  begin
    Result[0] := Vertex(R.Left, R.Top, 189, 230, 253, Transparent);
    Result[1] := Vertex(R.Right, R.Bottom, 166, 217, 244, Transparent);
  end
  else
  begin
    Result[0] := Vertex(R.Left, R.Top, 234, 246, 253, Transparent);
    Result[1] := Vertex(R.Right, R.Bottom, 215, 239, 252, Transparent);
  end;
end;

function TFWCustomVistaPathEdit.GetCustomVertex(R: TRect; Transparent: ShortInt;
  Value: TDoubleVertex): TBackgroundVertex;
begin
  if Transparent < 0 then
    Transparent := 0;
  Result[0] := Vertex(R.Left, R.Top, Value[0].Red,
    Value[0].Green, Value[0].Blue, Transparent);
  Result[1] := Vertex(R.Right, R.Bottom, Value[1].Red,
    Value[1].Green, Value[1].Blue, Transparent);
end;

function TFWCustomVistaPathEdit.GetDarkVertex(R: TRect; Transparent: ShortInt;
  Down: Boolean): TBackgroundVertex;
begin
  if Transparent < 0 then
    Transparent := 0;
  if Down then
  begin
    Result[0] := Vertex(R.Left, R.Top, 152, 209, 239, Transparent);
    Result[1] := Vertex(R.Right, R.Bottom, 99, 172, 211, Transparent);
  end
  else
  begin
    Result[0] := Vertex(R.Left, R.Top, 222, 237, 246, Transparent);
    Result[1] := Vertex(R.Right, R.Bottom, 196, 229, 246, Transparent);
  end;
end;

function TFWCustomVistaPathEdit.GetEditHandle: TWinControl;
begin
  Result := FEditHandle;
end;

function TFWCustomVistaPathEdit.GetGrayVertex(R: TRect; Transparent: ShortInt;
   Down: Boolean): TBackgroundVertex;
begin
  if Transparent < 0 then
    Transparent := 0;
  if Down then
  begin
    Result[0] := Vertex(R.Left, R.Top, 220, 220, 220, Transparent);
    Result[1] := Vertex(R.Right, R.Bottom, 207, 207, 207, Transparent);
  end
  else
  begin
    Result[0] := Vertex(R.Left, R.Top, 242, 242, 242, Transparent);
    Result[1] := Vertex(R.Right, R.Bottom, 234, 234, 234, Transparent);
  end;
end;

function TFWCustomVistaPathEdit.GetListHandle: TWinControl;
begin
  Result := FListHandle;
end;

procedure TFWCustomVistaPathEdit.ImageListChange(Sender: TObject);
begin
  DrawTabs;
  Invalidate;
end;

procedure TFWCustomVistaPathEdit.InitDropDownMarks;
var
  B: TBitmap;
begin
  FDropMarks := TImageList.Create(Self);
  B := TBitmap.Create;
  try
    B.PixelFormat := pf32bit;
    B.Width := 16;
    B.Height := 16;
    B.Canvas.Brush.Color := clWhite;
    B.Canvas.Pen.Color := clBlack;
    B.Canvas.FillRect(Rect(0, 0, 16, 16));
    B.Canvas.MoveTo(0, 0);
    B.Canvas.LineTo(0, 7);
    B.Canvas.MoveTo(1, 1);
    B.Canvas.LineTo(1, 6);
    B.Canvas.MoveTo(2, 2);
    B.Canvas.LineTo(2, 5);
    B.Canvas.Pixels[3, 3] := clBlack;
    ImageList_AddMasked(FDropMarks.Handle, B.Handle, clWhite);
    B.Canvas.FillRect(Rect(0, 0, 16, 16));    
    B.Canvas.MoveTo(0, 0);
    B.Canvas.LineTo(7, 0);
    B.Canvas.MoveTo(1, 1);
    B.Canvas.LineTo(6, 1);
    B.Canvas.MoveTo(2, 2);
    B.Canvas.LineTo(5, 2);
    B.Canvas.Pixels[3, 3] := clBlack;
    ImageList_AddMasked(FDropMarks.Handle, B.Handle, clWhite);
    B.Canvas.FillRect(Rect(0, 0, 16, 16));
    B.Canvas.Pixels[2, 0] := clBlack;
    B.Canvas.Pixels[3, 0] := clBlack;
    B.Canvas.Pixels[6, 0] := clBlack;
    B.Canvas.Pixels[7, 0] := clBlack;
    B.Canvas.Pixels[1, 1] := clBlack;
    B.Canvas.Pixels[2, 1] := clBlack;
    B.Canvas.Pixels[5, 1] := clBlack;
    B.Canvas.Pixels[6, 1] := clBlack;
    B.Canvas.Pixels[0, 2] := clBlack;
    B.Canvas.Pixels[1, 2] := clBlack;
    B.Canvas.Pixels[4, 2] := clBlack;
    B.Canvas.Pixels[5, 2] := clBlack;
    B.Canvas.Pixels[1, 3] := clBlack;
    B.Canvas.Pixels[2, 3] := clBlack;
    B.Canvas.Pixels[5, 3] := clBlack;
    B.Canvas.Pixels[6, 3] := clBlack;
    B.Canvas.Pixels[2, 4] := clBlack;
    B.Canvas.Pixels[3, 4] := clBlack;
    B.Canvas.Pixels[6, 4] := clBlack;
    B.Canvas.Pixels[7, 4] := clBlack;
    ImageList_AddMasked(FDropMarks.Handle, B.Handle, clWhite);
  finally
    B.Free;
  end;
end;

procedure TFWCustomVistaPathEdit.InitTabs;
var
  S: TStringlist;
  I, FreeSpace, OffsetCursor: Integer;
begin
  FHiddenCount := 0;
  S := TStringList.Create;
  try
    S.Text := StringReplace(FPath, '\', sLineBreak, [rfReplaceAll]);

    // Рассчитываем позиции всех табов
    SetLength(FTabs, S.Count + 2);
    FTabs[0].Width := LEFT_BTN_WIDTH;
    FTabs[0].HasDropDown := True;
    for I := 0 to S.Count - 1 do
    begin
      FTabs[I + 1].Hidden := False;
      FTabs[I + 1].Caption := S[I];
      FTabs[I + 1].Width := BTN_NC_DOUBLE_WIDTH + BTN_DROPDOWN_WIDTH +
        Canvas.TextWidth(FTabs[I + 1].Caption);
      FTabs[I + 1].LeftOffset := FTabs[I].LeftOffset + FTabs[I].Width + 1;
      FTabs[I + 1].HasDropDown := True;
    end;

    // Смотрим есть ли папки у самого последнего элемента
    DoHasFolder(S.Count, FTabs[S.Count].HasDropDown);
    if not FTabs[S.Count].HasDropDown then
      Dec(FTabs[S.Count].Width, BTN_DROPDOWN_WIDTH);

    FTabs[S.Count + 1].LeftOffset := ClientWidth - RIGHT_BTN_WIDTH - 1;
    FTabs[S.Count + 1].Width := RIGHT_BTN_WIDTH;
    FTabs[S.Count + 1].HasDropDown := False;
    FTabs[S.Count + 1].Caption := '';
    FTabs[S.Count + 1].Hidden := False;

    // Смотрим кол-во свободного места
    FreeSpace := ClientWidth - LEFT_BTN_WIDTH - RIGHT_BTN_WIDTH -
      EMPTY_SECTOR_WIDTH - (FTabs[S.Count].LeftOffset + FTabs[S.Count].Width);
    if FreeSpace > 0 then Exit;

    // Если свободного места меньше чем необходимо, скрываем первые табы
    FreeSpace := FreeSpace * -1;
    OffsetCursor := LEFT_BTN_WIDTH + 1;
    for I := 1 to S.Count do
      if FTabs[I].LeftOffset > FreeSpace + LEFT_BTN_WIDTH then
      begin
        FTabs[I].LeftOffset := OffsetCursor;
        Inc(OffsetCursor, FTabs[I].Width + 1);
      end
      else
      begin
        FTabs[I].Hidden := True;
        Inc(FHiddenCount);
      end;

    // Проверяем чтобы последний значащий таб был всегда виден
    I := Length(FTabs) - 2;
    if FTabs[I].Hidden then
    begin
      Dec(FHiddenCount);
      FTabs[I].Hidden := False;
      FreeSpace := FreeSpace + LEFT_BTN_WIDTH - FTabs[I].LeftOffset;
      FTabs[I].LeftOffset := LEFT_BTN_WIDTH + 1;
      // А если даже последний таб слишком большой,
      // тогда уменьшаем его ширину до самой минимальной
      if FTabs[I].Width > FreeSpace then
        if FTabs[I].Width - FreeSpace > BTN_MIN_WIDTH then
          FTabs[I].Width := FTabs[I].Width - FreeSpace
        else
          if FTabs[I].Width > BTN_MIN_WIDTH then
            FTabs[I].Width := BTN_MIN_WIDTH;
    end;

  finally
    S.Free;
  end;
end;

function TFWCustomVistaPathEdit.IsPopupShowing: Boolean;
begin
  Result := False;
  if csDesigning in ComponentState then Exit;
  Result := FPopupHandle.Visible;
end;

procedure TFWCustomVistaPathEdit.Loaded;
begin
  inherited;
  TabStop := True;
  Resize;
end;

procedure TFWCustomVistaPathEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NeedRepaint: Boolean;
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  FMousePressed := True;
  if not (csDesigning in ComponentState) then
    if CanFocus then SetFocus;  
  if EditState = esEdit then Exit;
  if FCurrentElement.TabIndex = -1 then Exit;
  NeedRepaint := False;
  if FCurrentElement.Element = elRightDropDown then
    DropDownHistory
  else
    case FTabs[FCurrentElement.TabIndex].State of
      tsHot:
      begin
        FTabs[FCurrentElement.TabIndex].State := tsPressed;
        NeedRepaint := True;
      end;
      tsDropDownHot:
        DropDownPopup;
    end;
  if NeedRepaint then
  begin
    DrawTabs;
    Invalidate;
  end;    
end;

procedure TFWCustomVistaPathEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  NeedRepaint, NeedTimer: Boolean;
  NewState: TTabState;
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if FMousePressed then Exit;
  if EditState = esCombo then Exit;

  NeedRepaint := False;
  NeedTimer := False;
  FCurrentElement := QueryElementAtPos(X);
  for I := 0 to Length(FTabs) - 1 do
  begin
    // Проверка - над каким табом мы сейчас находимся?
    if FCurrentElement.TabIndex = I then
    begin

      // Таб активен

      // При активном попапе - вся анимация отключена
      if IsPopupShowing then
      begin
        FTabs[FLastPopupTabIndex].State := tsPressed;
        FTabs[I].Direction := 0;
        FTabs[I].Transparent := 0;
        if FCurrentElement.Element <> elRightDropDown then
        begin
          DropDownPopup;
          Continue;
        end;
      end;

      // Нашли таб, смотрим где именно? Над табом или кнопкой?
      NewState := tsInactive;
      if FCurrentElement.Element in [elLeftBtn, elTab, elRightDropDown] then
        NewState := tsHot;
      if FCurrentElement.Element in [elLeftDropDown, elTabDropDown] then
        NewState := tsDropDownHot;

      // Если состояние изменилось,
      // выставляем флаг о необходимости перерисовки
      if FTabs[I].State <> NewState then
        NeedRepaint := True;

      // Если таб был до этого неактивен (или тушился), включаем анимацию
      // (0 - полная непрозрачность, 5 - полная прозрачность)
      if (FTabs[I].State = tsInactive) or (FTabs[I].Direction = 1) then
      begin
        // Проверка - не находится ли таб в состоянии средней прозрачности?
        // Такое возможно если быстро убрать курсор с таба, он начнет тушится
        // и при этом заново вернуть курсор на место
        if FTabs[I].Direction = 0 then
        begin
          FTabs[I].Transparent := 5;
          // Включаем флаг о необходимости активации таймера
          NeedTimer := True;
        end;
        // Включаем направление изменения прозрачности
        FTabs[I].Direction := -1;
      end;
      FTabs[I].State := NewState;
      Continue;
    end;

    // Таб неактивен

    // При активном попапе - вся анимация отключена
    if IsPopupShowing then
    begin
      if I <> FLastPopupTabIndex then
      begin
        if FTabs[I].State <> tsInactive then
        begin
          FTabs[I].State := tsInactive;
          NeedRepaint := True;
        end;
        FTabs[I].Direction := 0;
        FTabs[I].Transparent := 0;
      end;
      Continue;
    end;

    // Активируем анимацию затухания
    // В состояние tsInactive он перейдет после окончания цикла анимации
    if FTabs[I].State <> tsInactive then
    begin
      if FTabs[I].Direction = 0 then
      begin
        NeedTimer := True;
        FTabs[I].State := tsHot;
      end;
      FTabs[I].Direction := 1;
    end;
  end;

  // Отрисовываем изменения по необходимости
  if NeedRepaint then
  begin
    DrawTabs;
    Invalidate;
  end;

  // Если требуется анимация - активируем таймер
  if NeedTimer then
    SetTimer(Handle, 1, 50, nil);
end;

procedure TFWCustomVistaPathEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  APath: string;
  Accept: Boolean;
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  FMousePressed := False;
  if FCurrentElement.TabIndex <> QueryElementAtPos(X, Y).TabIndex then
    MouseMove(Shift, X, Y)
  else
  case FCurrentElement.Element of
    elLeftBtn, elEditRect: EditState := esEdit;
    elTab:
    begin
      APath := TabIndexToString(FCurrentElement.TabIndex);
      Accept := True;
      DoNewPath(FCurrentElement.TabIndex, Accept);
      if Accept then
        Text := APath;
    end;                    
  end;
end;

procedure TFWCustomVistaPathEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then Images := nil;
    if AComponent = ExternalPainter then ExternalPainter := nil;
  end;
end;

procedure TFWCustomVistaPathEdit.Paint;
begin
  inherited;
  BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
    FBackGround.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TFWCustomVistaPathEdit.PerformMessageToCombo(var Message: TMessage);
begin
  if csDesigning in ComponentState then Exit;
  if FListHandle.Visible then  
    with Message do
      Result := SendMessage(FListHandle.Handle, Msg, WParam, LParam);
end;

function TFWCustomVistaPathEdit.QueryElementAtPos(X: Integer; Y: Integer = -1): TQueryElement;
var
  I, A: Integer;
begin
  Result.TabIndex := -1;
  Result.Element := elNone;
  if Y > Height then Exit;
  Result.Element := elEditRect;
  A := Length(FTabs) - 1;
  if X >= FTabs[A].LeftOffset then
  begin
    Result.TabIndex := A;
    Result.Element := elRightDropDown;
    Exit;
  end;
  for I := 0 to A do
  begin
    if FTabs[I].Hidden then Continue;
    if X >= FTabs[I].LeftOffset then
      if X < FTabs[I].LeftOffset + FTabs[I].Width then
      begin
        Result.TabIndex := I;
        if I = 0 then        
          Result.Element := elLeftBtn
        else
          Result.Element := elTab;
        if FTabs[I].HasDropDown then
          if X >= FTabs[I].LeftOffset + FTabs[I].Width - BTN_DROPDOWN_WIDTH then
            if I = 0 then
              Result.Element := elLeftDropDown
            else
              Result.Element := elTabDropDown;
        Break;
      end;
  end;
end;

procedure TFWCustomVistaPathEdit.Resize;
begin
  inherited;
  FBackGround.Width := ClientWidth;
  FBackGround.Height := ClientHeight;
  if not (csDesigning in ComponentState) then
  begin
    FEditHandle.Left := 20;
    FEditHandle.Top := 2;
    FEditHandle.Height := ClientHeight - 2;
    FEditHandle.Width := ClientWidth - FEditHandle.Left - RIGHT_BTN_WIDTH - 1;
  end;
  if Length(FTabs) > 0 then  
    FTabs[Length(FTabs) - 1].LeftOffset := ClientWidth - RIGHT_BTN_WIDTH - 1;
  InitTabs;
  DrawTabs;
end;

procedure TFWCustomVistaPathEdit.SetEditState(const Value: TGSEditState);
begin
  if EditState <> Value then
  begin
    FEditState := Value;
    if csDesigning in ComponentState then Exit;
    case Value of
      esEdit, esCombo, esAutoCompletion:
      begin
        if Value <> esAutoCompletion then
          TSubEdit(FEditHandle).Text := FPath;
        FEditHandle.Visible := True;
        FEditHandle.SetFocus;
      end;
      esTab:
      begin
        CloseDropDown;
        FEditHandle.Visible := False;
      end;
    end;
    InitTabs;
    DrawTabs;
    Invalidate;
  end; 
end;

procedure TFWCustomVistaPathEdit.SetEditText(const Value: string);
begin
  if csDesigning in ComponentState then Exit;
  TSubEdit(FEditHandle).Text := Value;
  TSubEdit(FEditHandle).SelStart := Length(Value);
end;

procedure TFWCustomVistaPathEdit.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  DrawTabs;
  Invalidate;
end;

procedure TFWCustomVistaPathEdit.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    Invalidate;
  end
end;

procedure TFWCustomVistaPathEdit.SetMenuPainter(const Value: TAbstractPainter);
begin
  if ExternalPainter <> Value then
  begin
    FPainter := Value;
    if Value <> nil then
    begin
      Value.Images := Images;
      if smMenuPaint in Value.SupportedMedods then
        TSubPopupMenu(FPopupHandle).OnPaintItem := Value.MenuPaint
      else
        TSubPopupMenu(FPopupHandle).OnPaintItem := nil;
      if smMenuFillBackground in Value.SupportedMedods then
        TSubPopupMenu(FPopupHandle).OnFillBackground := Value.MenuFillBackground
      else
        TSubPopupMenu(FPopupHandle).OnFillBackground := nil;
      Value.OnChange := DoPainterChange;
    end
    else
    begin
      TSubPopupMenu(FPopupHandle).OnPaintItem := nil;
      TSubPopupMenu(FPopupHandle).OnFillBackground := nil;
    end;
  end;
end;

procedure TFWCustomVistaPathEdit.SetPath(const Value: string);
begin
  if FPath <> Value then
  begin
    FPath := Value;
    if not (csDesigning in ComponentState) then
      TCustomEdit(FEditHandle).Text := Value;
    InitTabs;
    DrawTabs;
    Invalidate;
  end;
end;

procedure TFWCustomVistaPathEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS;
end;

procedure TFWCustomVistaPathEdit.WMTime(var Message: TMessage);
var
  I: Integer;
  StopTimer: Boolean;
begin
  StopTimer := True;
  for I := 0 to Length(FTabs) - 1 do
    if FTabs[I].Direction <> 0 then
    begin
      Inc(FTabs[I].Transparent, FTabs[I].Direction);
      if FTabs[I].Direction > 0 then
      begin
        if FTabs[I].Transparent < 5 then
          StopTimer := False
        else
        begin
          FTabs[I].Direction := 0;
          FTabs[I].Transparent := 0;
          FTabs[I].State := tsInactive;
        end;
      end
      else
        if FTabs[I].Transparent > 0 then
          StopTimer := False
        else
          FTabs[I].Direction := 0;
    end;
  if StopTimer {or not FMouseHere} then
    KillTimer(Handle, 1);
  DrawTabs;
  Invalidate;
end;

procedure TFWCustomVistaPathEdit.WndProc(var Message: TMessage);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  case Message.Msg of
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
    begin
      with TMessage(Message) do
        SendMessage(FEditHandle.Handle, Msg, WParam, LParam);
      if FListHandle.Visible then
        with TMessage(Message) do
          SendMessage(FListHandle.Handle, Msg, WParam, LParam);
      if IsPopupShowing then
        with TMessage(Message) do
          SendMessage(FPopupHandle.Handle, Msg, WParam, LParam);      
    end;
    WM_SETFOCUS:
      if FListHandle.Visible then
        with TMessage(Message) do
          SendMessage(FListHandle.Handle, Msg, WParam, LParam);
  end;
end;

{ TAbstractPainter }

procedure TAbstractPainter.DoChanged;
begin
  if Assigned(FChange) then
    FChange(Self);
end;

procedure TAbstractPainter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FImages then FImages := nil;    
end;

end.
