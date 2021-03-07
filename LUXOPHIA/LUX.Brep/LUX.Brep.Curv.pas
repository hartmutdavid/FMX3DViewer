unit LUX.Brep.Curv;

interface //#################################################################### ■

uses LUX, LUX.Data.Tree, LUX.Brep.Poin;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurv<_TPos_>

     TCurv<_TPos_ :record;
           _TPoin_:TPoin<_TPos_>> = class( TTreeLeaf<TTreeNode> )
     private
     protected
       _Poins :TArray<_TPoin_>;
       ///// アクセス
       function GetPoins( const I_:Integer ) :_TPoin_; virtual;
       procedure SetPoins( const I_:Integer; const Poin_:_TPoin_ ); virtual;
       function GetPoinsN :Integer; virtual;
       procedure SetPoinsN( const PoinsN_:Integer ); virtual;
     public
       ///// プロパティ
       property Poins[ const I_:Integer ] :_TPoin_ read GetPoins  write SetPoins ;
       property PoinsN                    :Integer read GetPoinsN write SetPoinsN;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurvModel<_TPos_,_TPoin_,_TCurv_>

     TCurvModel<_TPos_ :record;
                _TPoin_:TPoin<_TPos_>;
                _TCurv_:TCurv<_TPos_,_TPoin_>> = class( TTreeRoot<_TCurv_> )
     private
     protected
       _PoinModel :TPoinModel<_TPos_,_TPoin_>;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property PoinModel :TPoinModel<_TPos_,_TPoin_> read _PoinModel;
       ///// メソッド
       procedure DeleteChilds; override;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurv<_TPos_,_TPoin_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TCurv<_TPos_,_TPoin_>.GetPoins( const I_:Integer ) :_TPoin_;
begin
     Result := _Poins[ I_ ];
end;

procedure TCurv<_TPos_,_TPoin_>.SetPoins( const I_:Integer; const Poin_:_TPoin_ );
begin
     _Poins[ I_ ] := Poin_;
end;

//------------------------------------------------------------------------------

function TCurv<_TPos_,_TPoin_>.GetPoinsN :Integer;
begin
     Result := Length( _Poins );
end;

procedure TCurv<_TPos_,_TPoin_>.SetPoinsN( const PoinsN_:Integer );
begin
     SetLength( _Poins, PoinsN_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurvModel<_TPos_,_TPoin_,_TCurv_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TCurvModel<_TPos_,_TPoin_,_TCurv_>.Create;
begin
     inherited;

     _PoinModel := TPoinModel<_TPos_,_TPoin_>.Create;
end;

destructor TCurvModel<_TPos_,_TPoin_,_TCurv_>.Destroy;
begin
     _PoinModel.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TCurvModel<_TPos_,_TPoin_,_TCurv_>.DeleteChilds;
begin
     inherited DeleteChilds;

     PoinModel.DeleteChilds;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
