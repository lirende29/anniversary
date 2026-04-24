# -*- coding: utf-8 -*-
"""
部分仿真过程，完整仿真通过调整参数范围和不同类别完成
"""
import numpy as np
import pandas as pd
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm
import os

def calculate_entropy(probabilities):
    return -np.sum(probabilities * np.log(probabilities))

# 计算Jensen-Shannon Divergence的函数
def jensen_shannon_divergence(p1, p2):
    p1 = np.array(p1)
    p2 = np.array(p2)
    p1 = np.clip(p1, 1e-10, 1.0)
    p2 = np.clip(p2, 1e-10, 1.0)
    m = 0.5 * (p1 + p2)
    h1 = calculate_entropy(p1)
    h2 = calculate_entropy(p2)
    hm = calculate_entropy(m)
    jsd = hm - 0.5 * (h1 + h2)
    return jsd

# 模拟数据生成函数
def simulate_data(alpha, r, q, tm=150, days=300):
    H_0 = 10
    n = []
    me = []
    for t in range(1, days):
        distance_to_memorial_day = t - tm
        memorial_effect = (1450) * ((abs(distance_to_memorial_day) + 1)) **(-r)
        me.append(memorial_effect)
        H_0 = (H_0 * alpha + memorial_effect)
        n.append(H_0)
    H_0 = n[298]
    n = []
    me = []
    for t in range(1, days):
        distance_to_memorial_day = t - tm
        memorial_effect = (1450) * ((abs(distance_to_memorial_day) + 1)) ** (-r)
        me.append(memorial_effect)
        H_0 = (H_0 * alpha + memorial_effect)
        n.append(H_0)
    n = [item + q for item in n]
    n = [item / max(n) for item in n]  # 归一化
    return n

# 并行计算JSD的函数
def calculate_jsd_for_params(params, combined_column):
    alpha, r, q = params
    n_simulation = simulate_data(alpha, r, q)
    jsd_value = jensen_shannon_divergence(combined_column, n_simulation)
    return {
        'alpha': alpha,
        'r': r,
        'q': q,
        'JSD': jsd_value
    }

# 分批处理函数
def process_batch(batch, combined_column, batch_num, output_dir):
    results = []
    with ProcessPoolExecutor() as executor:
        futures = [executor.submit(calculate_jsd_for_params, params, combined_column) for params in batch]
        
        for future in tqdm(as_completed(futures), total=len(futures), desc=f"处理批次 {batch_num}"):
            result = future.result()
            if result is not None:
                results.append(result)
    
    # 保存当前批次的结果
    if results:
        batch_df = pd.DataFrame(results)
        output_path = os.path.join(output_dir, f'LL_JSD_results_batch_{batch_num+1320}.csv')
        batch_df.to_csv(output_path, index=False)
        print(f"批次 {batch_num+1320} 完成，结果已保存到 {output_path}")

# 主函数
def main(combined_column, output_dir='C:/Users/21304/Desktop/HH1_200/LL_batch_results'):
    # 创建输出目录
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # 定义参数范围
    alpha_values = np.linspace(-1, 0, 101)
    r_values = np.linspace(1, 10, 91)
    q_values = np.linspace(3000, 3900, 901)
    
    # 生成所有可能的参数组合
    all_combinations = [(alpha, r, q) for alpha in alpha_values for r in r_values for q in q_values]
    
    # 设置每批次的组合数量
    batch_size = 5000  # 可以根据内存大小调整
    
    # 分批处理
    total_batches = len(all_combinations) // batch_size + 1
    for batch_num in range(total_batches):
        start_idx = batch_num * batch_size
        end_idx = start_idx + batch_size
        current_batch = all_combinations[start_idx:end_idx]
        
        print(f"\n开始处理批次 {batch_num + 1}/{total_batches} (参数组合 {start_idx}-{end_idx})")
        process_batch(current_batch, combined_column, batch_num + 1, output_dir)
    
    # 合并所有批次的结果
    combine_results(output_dir)

# 合并结果的函数
def combine_results(output_dir):
    all_files = [f for f in os.listdir(output_dir) if f.startswith('JSD_results_batch_')]
    combined_df = pd.DataFrame()
    
    for file in all_files:
        file_path = os.path.join(output_dir, file)
        df = pd.read_csv(file_path)
        combined_df = pd.concat([combined_df, df], ignore_index=True)
    
    # 保存合并后的结果
    combined_output = os.path.join(output_dir, 'JSD_results_combined.csv')
    combined_df.to_csv(combined_output, index=False)
    print(f"\n所有批次处理完成，合并结果已保存到 {combined_output}")
    
    # 找到JSD最小的参数组合
    min_jsd_row = combined_df.loc[combined_df['JSD'].idxmin()]
    print("\n最优参数组合:")
    print(min_jsd_row)

if __name__ == '__main__':
    # 加载数据
    combined_df = pd.read_csv("C:/Users/21304/Desktop/HH1_200/LL.csv")
    combined_column = combined_df.iloc[:, 0].values  # 转换为 numpy 数组
    
    # 调用主函数
    main(combined_column)






"""
最优解查询
"""
# 设置批次结果所在的目录
output_dir = "C:/Users/21304/Desktop/HH1_200/LH_batch_results"

# 初始化最小 JSD 和对应参数
min_jsd = float('inf')  # 初始设为无穷大
best_alpha = None
best_r = None
best_q = None

# 遍历所有批次文件
for file in os.listdir(output_dir):
    if file.startswith('LH_JSD_results_batch_') and file.endswith('.csv'):
        file_path = os.path.join(output_dir, file)
        df = pd.read_csv(file_path)
        
        # 检查列名是否包含 'JSD'（兼容大小写或不同列名）
        jsd_col = None
        for col in df.columns:
            if col.lower() == 'jsd':  # 兼容大小写
                jsd_col = col
                break
        
        if jsd_col is None:
            print(f"警告：文件 {file} 中没有找到 JSD 列，跳过该文件。")
            continue
        
        # 找到当前文件中的最小 JSD
        current_min_jsd = df[jsd_col].min()
        if current_min_jsd < min_jsd:
            min_jsd = current_min_jsd
            best_row = df.loc[df[jsd_col].idxmin()]
            best_alpha = best_row['alpha']
            best_r = best_row['r']
            best_q = best_row['q']

# 输出全局最优参数
print("全局最优参数组合:")
print(f"alpha: {best_alpha}")
print(f"r: {best_r}")
print(f"q: {best_q}")
print(f"最小 JSD: {min_jsd}")





"""
#画图-四类仿真结果图和真实数据对比
HH.csv为HH类真实数据，其他类别同样。
"""
import matplotlib.pyplot as plt
combined_df = pd.read_csv('C:/Users/21304/Desktop/HH1_200/HH.csv')#JSD=0.0624035313556561
combined_column = combined_df.iloc[:, 0]
n=simulate_data(0.08,2.3,148)

combined_df2 = pd.read_csv('C:/Users/21304/Desktop/HH1_200/HL.csv')#JSD=0.0210209953888309
combined_column2 = combined_df2.iloc[:, 0]
n1=simulate_data(-0.13,1.8,125)

combined_df3 = pd.read_csv('C:/Users/21304/Desktop/HH1_200/LH.csv')#JSD=0.0656837619519876
combined_column3 = combined_df3.iloc[:, 0]
n2=simulate_data(0.53,20,2028)

combined_df4 = pd.read_csv('C:/Users/21304/Desktop/HH1_200/LL.csv')#JSD=0.1135659143869531
combined_column4 = combined_df4.iloc[:, 0]
n3=simulate_data(-0.42,1,3004)



# 创建子图
fig, axs = plt.subplots(2, 2, figsize=(10, 6))

# 第一个子图
axs[0, 0].scatter(range(-150, len(n) + 1-151), n, label='Simulation',
                  edgecolor='blue', facecolor='none', s=20)
axs[0, 0].scatter(range(-150, len(combined_column) + 1-151), combined_column, label='HH',
                  edgecolor='red', facecolor='none', s=20)
axs[0, 0].set_xlabel('Days')
axs[0, 0].set_ylabel('Frequence')
axs[0, 0].legend()

# 第二个子图
axs[0, 1].scatter(range(-150, len(n1) + 1-151), n1, label='Simulation',
                  edgecolor='blue', facecolor='none', s=20)
axs[0, 1].scatter(range(-150, len(combined_column2) + 1-151), combined_column2, label='HL',
                  edgecolor='red', facecolor='none', s=20)
axs[0, 1].set_xlabel('Days')
axs[0, 1].set_ylabel('Frequence')
axs[0, 1].legend()

# 第三个子图
axs[1, 0].scatter(range(-150, len(n2) + 1-151), n2, label='Simulation',
                  edgecolor='blue', facecolor='none', s=20)
axs[1, 0].scatter(range(-150, len(combined_column3) + 1-151), combined_column3, label='LH',
                  edgecolor='red', facecolor='none', s=20)
axs[1, 0].set_xlabel('Days')
axs[1, 0].set_ylabel('Frequence')
axs[1, 0].legend()

# 第四个子图
axs[1, 1].scatter(range(-150, len(n3) + 1-151), n3, label='Simulation',
                  edgecolor='blue', facecolor='none', s=20)
axs[1, 1].scatter(range(-150, len(combined_column4) + 1-151), combined_column4, label='LL',
                  edgecolor='red', facecolor='none', s=20)
axs[1, 1].set_xlabel('Days')
axs[1, 1].set_ylabel('Frequence')
axs[1, 1].legend()



plt.tight_layout()
plt.savefig('C:/Users/21304/Desktop/output_figure.pdf')
plt.show()

