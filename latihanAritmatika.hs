-- Membuat struktur data aritmatika sederhana, Expr
-- Let String Expr Expr memberi tahu bahwa jika ada suatu String Expr Expr itu bisa dikategorikan tipe data Expr
data Expr = C Float
            | Expr :+ Expr 
            | Expr :- Expr 
            | Expr :* Expr
            | Expr :/ Expr
            | V String
            | Let String Expr Expr
        deriving Show

-- Fungsi substitusi
subst :: String -> Expr -> Expr -> Expr
subst v0 e0 (V v1) = if (v0 == v1) then e0 else (V v1)
subst _ _ (C c) = (C c) 
subst v0 e0 (e1 :+ e2) = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :- e2) = subst v0 e0 e1 :- subst v0 e0 e2
subst v0 e0 (e1 :* e2) = subst v0 e0 e1 :* subst v0 e0 e2
subst v0 e0 (e1 :/ e2) = subst v0 e0 e1 :/ subst v0 e0 e2
subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2)
{-
Menurut saya subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2) tidak perlu koreksi karena cara kerja substitusinya sudah benar.
Jadi ada String v0, Expr e0, dan Expr (Let v1 e1 e2) sebagai input. outputnya adalah Let v1 e1 (subst v0 e0 e2), v0 dan e0 disubstitusi
dengan v1 dan e1
-}

-- Fungsi evaluasi original
evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2
evaluate (Let v e0 e1) = evaluate (subst v e0 e1)
evaluate (V _) = 0.0
-- Menurut saya evaluate (V _) sudah correct. evaluate menerima Expr (V _) dan _ bisa apa saja lalu mengembalikan 0.0
