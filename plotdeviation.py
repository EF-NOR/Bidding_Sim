import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


# --- List your CSV files here ---
FILES = [
    'Resultater_maisim/price_simulation_v2(R1).csv',
    'Resultater_maisim/price_simulation_v2(R2).csv',
    'Resultater_maisim/price_simulation_v2(R3).csv',
    'Resultater_maisim/price_simulation_v2(R4).csv ',
    'Resultater_maisim/price_simulation_v2(R5).csv',
]


def compute_daily_aad(filepath, reference='daily'):
    """
    Compute average absolute deviation per day.
    reference='daily'  -> deviation from each day's own mean
    reference='yearly' -> deviation from the full-year mean
    """
    df = pd.read_csv(filepath)
    df['time_oslo'] = pd.to_datetime(df['time_oslo'], utc=True)
    df['date'] = df['time_oslo'].dt.date

    if reference == 'daily':
        ref = df.groupby('date')['final_price'].transform('mean')
    elif reference == 'yearly':
        ref = df['final_price'].mean()
    else:
        raise ValueError("reference must be 'daily' or 'yearly'")

    df['abs_dev'] = (df['final_price'] - ref).abs()

    daily = df.groupby('date')['abs_dev'].mean().reset_index()
    daily['date'] = pd.to_datetime(daily['date'])
    return daily.set_index('date')['abs_dev']


# --- Load all series for both reference types ---
series_daily = [compute_daily_aad(f, reference='daily') for f in FILES]
series_yearly = [compute_daily_aad(f, reference='yearly') for f in FILES]

combined_daily = pd.concat(series_daily, axis=1)
combined_daily.columns = [f'Scenario {i+1}' for i in range(len(FILES))]

combined_yearly = pd.concat(series_yearly, axis=1)
combined_yearly.columns = [f'Scenario {i+1}' for i in range(len(FILES))]

# --- Compute averages and bands ---
avg_daily = combined_daily.mean(axis=1)
min_daily = combined_daily.min(axis=1)
max_daily = combined_daily.max(axis=1)
yearly_avg_of_daily = avg_daily.mean()

avg_yearly = combined_yearly.mean(axis=1)
min_yearly = combined_yearly.min(axis=1)
max_yearly = combined_yearly.max(axis=1)
yearly_avg_of_yearly = avg_yearly.mean()


def style_axis(ax):
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.grid(axis='y', linestyle='--', alpha=0.4)
    ax.tick_params(axis='both', which='major', labelsize=22)
    ax.xaxis.set_major_locator(plt.matplotlib.dates.MonthLocator())
    ax.xaxis.set_major_formatter(plt.matplotlib.dates.DateFormatter('%b'))


# --- Plot: two stacked subplots ---
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(16, 10), sharex=True)

# --- Top: AAD from same-day mean ---
ax1.fill_between(combined_daily.index, min_daily, max_daily,
                 color="#db5415", alpha=0.4, label='Min–max range')
ax1.plot(avg_daily.index, avg_daily, color='#1a5f7a',
         linewidth=1.8, label='Average')
ax1.axhline(yearly_avg_of_daily, color='#c0392b', linewidth=1.5, linestyle='--',
            label=f'Annual average: {yearly_avg_of_daily:.1f} EUR/MWh', zorder=5)
ax1.set_ylabel('AAD from daily mean\n(EUR/MWh)', fontsize=24)
ax1.legend(fontsize=22, frameon=False, ncol=3, loc='upper right')
ax1.set_ylim(0, 60)
style_axis(ax1)

# --- Bottom: AAD from yearly mean ---
ax2.fill_between(combined_yearly.index, min_yearly, max_yearly,
                 color="#db5415", alpha=0.4, label='Min–max range')
ax2.plot(avg_yearly.index, avg_yearly, color='#1a5f7a',
         linewidth=1.8, label='Average')
ax2.axhline(yearly_avg_of_yearly, color='#c0392b', linewidth=1.5, linestyle='--',
            label=f'Annual average: {yearly_avg_of_yearly:.1f} EUR/MWh', zorder=5)
ax2.set_ylabel('AAD from yearly mean\n(EUR/MWh)', fontsize=24)
ax2.legend(fontsize=22, frameon=False, ncol=3, loc='upper right')
ax2.set_ylim(0, 100)
style_axis(ax2)

plt.tight_layout()
plt.savefig('daily_deviation_2050.png', dpi=150, bbox_inches='tight')
plt.show()
print("Saved to daily_deviation_2050.png")